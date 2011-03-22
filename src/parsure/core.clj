(ns parsure.core
  (:use [parsure.char]))

; ;; Parser a :: String -> [a String]
; ;; return :: a -> Parser a
; (defn return [v] (fn [inp] (list v inp)))
; ;; failure :: Parser a
; (defn failure
  ; ([] (failure ""))
  ; ([msg] (fn [inp] nil)))
; ;; item :: Parser Char
; (defn item [inp]
  ; (if (= (count inp) 0)
    ; nil
    ; (list (first inp) (rest inp))))
; ;; parse :: Parser a -> String -> [a String]
; (defn parse [p inp] (p inp))
; ;; bind :: Parser a -> (a -> Parser b) -> Parser b
; (defn bind [p f]
  ; (fn [inp]
    ; (let [ret (parse p inp)]
      ; (if (nil? ret)
        ; nil
        ; (let [[v out] ret]
          ; (parse (f v) out))))))
; ;; Parser a -> Parser a -> Parser a
; (defn plus [p q]
  ; (fn [inp]
    ; (let [ret (parse p inp)]
      ; (if (nil? ret)
        ; (parse q inp)
        ; ret))))
; ;; (a -> b) -> Parser a -> Parser b
; (defn lift [f m]
  ; (dobind [x m]
          ; (return (f x))))

;; State
(defn new-state [inp] {:input inp :pos [0 0]})
(defn update-state [st k v] (assoc st k v))
(defn update-input [st inp] (update-state st :input inp))
(defn update-pos [st pos] (update-state st :pos pos))
(defn get-input [st] (st :input))
(defn get-pos [st] (st :pos))

;; Output
(defn success? [[rs v st]] (= rs 'success))
(defn failed? [[rs v st]] (= rs 'failed))
(defn eos? [[rs v st]] (= rs 'eos))
(defn get-value [[rs v st]] v)
(defn get-state [[rs v st]] st)

;; Primitives
;; - return :: a -> Parser a
(defn return [v] (fn [st] (list 'success v st)))
;; - failure :: String -> Parser a
(defn failure
  ([] (failure ""))
  ([msg] (fn [st] (list 'failed msg st))))
;; - item :: Parser Char
(defn item [st]
  (let [inp (get-input st)]
    (if (= (count inp) 0)
      (list 'failed nil st)
      (let [c (first inp)
            [col line] (get-pos st)
            new-pos [(if (= \newline c) 0 (+ col 1))
                     (if (= \newline c) (+ line 1) line)]
            new-st (update-input (update-pos st new-pos)
                                 (rest inp))]
        (list 'success c new-st)))))
;; - parse :: Parser a -> String -> [a state]
(defn parse [p st] ((force p) st))
;; - bind :: Parser a -> (a -> Parser b) -> Parser b
(defn bind [p f]
  (fn [st]
    (let [ret (parse p st)]
      (cond (success? ret) (parse (f (get-value ret))
                                  (get-state ret))
            :else ret))))
;; - plus :: Parser a -> Parser a -> Parser a
(defn plus [p q]
  (fn [st]
    (let [ret (parse p st)]
      (cond (success? ret) ret
            :else (parse q st)))))
;; - lift :: (a -> b) -> Parser a -> Parser b
(defn lift [f m]
  (bind m
        (fn [x] (return (f x)))))

;; Utilities
(defmacro defparser [p]
  `(delay ~p))

(defn- add-bind [m step]
  (let [[v expr] step]
    (list 'bind expr (list 'fn [v] m))))

(defmacro dobind [steps expr]
  (let [rsteps (reverse (partition 2 steps))]
    (reduce add-bind expr rsteps)))

(defmacro dobind_ [& steps]
  (let [rsteps (reverse (map #(list '_ %) steps))
        [_ ls] (first rsteps)]
    (reduce add-bind ls (rest rsteps))))

(defn- add-plus [m expr]
  (list 'plus expr m))

(defmacro doplus [& exprs]
  (let [rexprs (reverse exprs)]
    (reduce add-plus rexprs)))

(defmacro docatch [expr msg]
  `(doplus ~expr (failure ~msg)))

;; Chars
(defn satisfy [p]
  (dobind [x item]
          (let [ret (p x)]
            (if ret
              (return x)
              (failure "Not expected.")))))

(def digit (docatch (satisfy digit?)
                    "Expected digit."))
(def lower (docatch (satisfy lower?)
                    "Expected lower."))
(def upper (docatch (satisfy upper?)
                    "Expected upper."))
(def letter (docatch (satisfy letter?)
                     "Expected letter."))
(def alpha-num (docatch (satisfy #(or (digit? %) (letter? %)))
                        "Expected alphabet or number."))
(defn ch [x] (docatch (satisfy #(= x %))
                      (str "Expected char '" x "'")))

;; Combinators
(declare many many1)
(defn many [p]
  (plus (many1 p)
        (return nil)))
(defn many1 [p]
  (dobind [v p
           vs (many p)]
          (return (cons v vs))))

(defn one-of [s]
  (dobind [x item]
          (if ((set s) x)
            (return x)
            (failure))))
(defn none-of [s]
  (dobind [x item]
          (if ((set s) x)
            (failure)
            (return x))))

(declare sep-by sep-by-1)
(defn sep-by [p sep]
  (plus (sep-by-1 p sep) (return nil)))
(defn sep-by-1 [p sep]
  (dobind [x p
           xs (many (dobind_ sep p))]
          (return (cons x xs))))

(declare sep-end-by sep-end-by-1)
(defn sep-end-by [p sep]
  (plus (sep-end-by-1 p sep) (return nil)))
(defn sep-end-by-1 [p sep]
  (dobind [x p]
          (plus (dobind [_ sep
                         xs (sep-end-by p sep)]
                        (return (cons x xs)))
                (return (list x)))))

(declare end-by end-by-1)
(defn end-by [p sep]
  (many (dobind [x p
                 _ sep]
                (return x))))
(defn end-by-1 [p sep]
  (many1 (dobind [x p
                  _ sep]
                 (return x))))

;; Tokens
(defn string
  ([s]
   (if (= 0 (count s))
     (return "")
     (dobind [_ (ch (first s))
              _ (string (subs s 1))]
             (return s)))))

(def ident (dobind [x lower
                    xs (many alpha-num)]
                   (return (apply str (cons x xs)))))
(def nat (dobind [xs (many1 digit)]
                 (return (Integer/parseInt (apply str xs)))))
(def space (dobind [_ (many (satisfy #(Character/isSpaceChar %)))]
                   (return nil)))
(def whitespace (dobind [_ (many (satisfy #(Character/isWhitespace %)))]
                        (return nil)))

(defn lexeme [p] (dobind [v p
                          _ whitespace]
                         (return v)))

(def identifier (lexeme ident))
(def natural (lexeme nat))
(defn sym [s] (lexeme (string s)))

;; Parse interfaces
(defn parse-from-string [p inp] (parse p (new-state inp)))
(defn parse-test [p inp]
  (let [ret (parse-from-string p inp)
        value (get-value ret)
        state (get-state ret)
        [col line] (get-pos state)]
    (if (failed? ret)
      (println (str value " (" "col:" col " line:" line ")"))
      value)))

