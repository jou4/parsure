(ns parsure.core
  (:use [parsure pos error])
  (:require [clojure.contrib.monads :as m]
            [parsure.monad-ext :as me]
            [clojure.contrib.duck-streams :as ds]
            [clojure.contrib.stream-utils :as su]))

;; Show protocol
(defprotocol Show (show [this]))
(extend-protocol Show java.lang.Character (show [c] (str \\ c)))
(extend-protocol Show parsure.error.ParseError (show [err] (show-error err)))


;; Stream
(su/defstream java.io.BufferedReader [s]
  (let [line (.readLine s)]
    (if (nil? line)
      [nil nil]
      [(str line \newline) s])))


;; ParseError
(defn unknown-error [st] (new-error-unknown (:pos st)))

(defn unexpect-error [msg pos] (new-error-message (list 'SysUnExpect msg) pos))


;; State
(defrecord State [input ^parsure.pos.SourcePos pos user])

(defn initial-state [name input] (State. input (initial-pos name) nil))

(def state-input :input)
(def state-pos   :pos)
(def state-user  :user)

(defn update-parser-state [f]
  (fn [st cok cerr eok eerr]
    (let [new-st (f st)]
      (eok new-st new-st (unknown-error new-st)))))

(defn get-parser-state [] (update-parser-state identity))

(defn set-parser-state [st] (update-parser-state (fn [_] st)))


;; Running parser
(defn run-parser [p ^State st cok cerr eok eerr]
  #((force p) st cok cerr eok eerr))

(defn do-parse [p name inp]
  (let [cok  (fn [x st err] (list 'Right x))      ; Consumed & OK
        cerr (fn [err]      (list 'Left err))     ; Comsumed & Error
        eok  (fn [x st err] (list 'Right x))      ; Empty    & OK
        eerr (fn [err]      (list 'Left err))]    ; Empty    & Error
    (trampoline run-parser p (initial-state name inp) cok cerr eok eerr)))

(defmacro parse
  ([p inp] `(parse ~p "" ~inp))
  ([p name inp] `(m/with-monad parser-m (do-parse ~p ~name ~inp))))

;; read from anything which clojure.contrib.duck-streams can convert reader
(defmacro parse-from-any
  ([p any] `(parse-from-any ~p "" ~any))
  ([p name any] `(parse ~p ~name (su/stream-seq (su/stream-flatten (ds/reader ~any))))))

(defmacro parse-from-file [p path] `(parse-from-any ~p ~path ~path))


;; Define parser combinator
(defmacro defparser
  ([name body] `(def ~name (delay (m/with-monad parser-m ~body))))
  ([name args body] `(defn ~name ~args (m/with-monad parser-m ~body))))


;; Parser monad
(m/defmonad parser-m
  [
   m-result (fn [x]
              (fn [st cok cerr eok eerr]
                (eok x st (unknown-error st))))

   m-zero   (fn [st cok cerr eok eerr]
              (eerr (unknown-error st)))

   m-bind   (fn [m k]
              (fn [st cok cerr eok eerr]
                (let [mcok  (fn [x st err]
                              (let [pcok  cok
                                    pcerr cerr
                                    peok  (fn [x st- err-] (cok x st- (merge-error err err-)))
                                    peerr (fn [err-] (cerr (merge-error err err-)))]
                                (run-parser (k x) st pcok pcerr peok peerr)))
                      mcerr cerr
                      meok  (fn [x st err]
                              (let [pcok  cok
                                    pcerr cerr
                                    peok  (fn [x st- err-] (eok x st- (merge-error err err-)))
                                    peerr (fn [err-] (eerr (merge-error err err-)))]
                                (run-parser (k x) st pcok pcerr peok peerr)))
                      meerr eerr]
                  (run-parser m st mcok mcerr meok meerr))))

   m-plus   (fn [m n]
              (fn [st cok cerr eok eerr]
                (let [meerr
                      (fn [err]
                        (let [neok  (fn [x st- err-] (eok x st- (merge-error err err-)))
                              neerr (fn [err-] (eerr (merge-error err err-)))]
                          (run-parser n st cok cerr neok neerr)))]
                  (run-parser m st cok cerr eok meerr))))
   ])


(defn token-prim [show-token next-pos consume?]
  (fn [st cok cerr eok eerr]
    (let [input (state-input st)
          pos   (state-pos   st)
          user  (state-user  st)]
      (if (empty? input)
        (eerr (unexpect-error "" pos))
        (let [item   (first input)
              remain (rest  input)]
          (if (consume? item)
            (let [newpos (next-pos pos item remain)
                  newst  (State. remain newpos user)]
              (cok item newst (new-error-unknown newpos)))
            (eerr (unexpect-error (show-token item) pos))))))))

(defn many-accum [acc p]
  (fn [st cok cerr eok eerr]
    (letfn [(many-err [_ _ _]
                      (throw (Exception. "combinator 'many' is applied to a parser that accepts an empty string.")))
            (walk [coll item st- err]
                  (run-parser p
                              st-
                              (partial walk (acc item coll))
                              cerr
                              many-err
                              (fn [e] (cok (acc item coll) st- e))))]
      (run-parser p
                  st
                  (partial walk (seq []))
                  cerr
                  many-err
                  (fn [e] (eok [] st e))))))

(defn not-followed-by [p]
  (fn [st cok cerr eok eerr]
    (letfn [(pcok [x st- err] (eerr (new-error-message (list 'UnExpect (show x))
                                                       (state-pos st))))
            (peerr [err] (cok nil st nil))]
      (run-parser p st pcok cerr eok peerr))))

(defn try [p]
  (fn [st cok cerr eok eerr]
    (letfn [(pcerr [err] (eerr (set-error-pos (state-pos st) err)))]
      (run-parser p st cok pcerr eok eerr))))

(defn unexpected [msg]
  (fn [st cok cerr eok eerr]
    (eerr (new-error-message (list 'UnExpect msg) (state-pos st)))))

(defn- set-expect-errors [err msgs]
  (cond (empty? msgs) (set-error-pos (list 'Expect "") err)
        (= 1 (clojure.core/count msgs)) (set-error-message (list 'Expect (first msgs)) err)
        :else (let [fmsg (first msgs)
                    rmsgs (reverse (rest msgs))]
                (reduce (fn [err- msg-] (add-error-message (list 'Expect msg-) err-))
                        (set-error-message (list 'Expect fmsg) err)
                        rmsgs))))

(defn label [p msg]
  (fn [st cok cerr eok eerr]
    (letfn [(peok [x st- err] (eok x st- (if (error-is-unknown err)
                                           err
                                           (set-expect-errors err [msg]))))
            (peerr [err] (eerr (set-expect-errors err [msg])))]
      (run-parser p st cok cerr peok peerr))))


(m/defmonadfn <|> [p q] (m-plus p q))
(defmacro <?> [p msg] `(label ~p ~msg))


;; Parser state combinators
(m/with-monad parser-m

  (defn get-position []
    (m/domonad [st (get-parser-state)]
      (state-pos st)))

  (defn get-input []
    (m/domonad [st (get-parser-state)]
      (state-input st)))

  (defn set-position [pos]
    (update-parser-state
      (fn [{:keys [input _ user]}] (State. input pos user))))

  (defn set-input [input]
    (update-parser-state
      (fn [{:keys [_ pos user]}] (State. input pos user))))

  (defn update-state [f]
    (m/m-fmap state-user
              (update-parser-state
                (fn [{:keys [input pos user]}] (State. input pos (f user))))))

  (defn get-state [] (m/m-fmap state-user (get-parser-state)))

  (defn set-state [user] (update-state (fn [_] user)))

  )
