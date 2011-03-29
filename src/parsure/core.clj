(ns parsure.core
  (:use [clojure.contrib.monads]
        [parsure.monad-ext])
  (:require [clojure.contrib.str-utils2 :as su]))


;; Parser state
(defn new-state [input]
  {:input input
   :pos   [0 0]})

;; Do parse
(defn parse [p st] ((force p) st))
(defn parse-from-string [p inp] (parse p (new-state inp)))

;; Define parser combinator
(defmacro defparser
  ([name body] `(def ~name (delay (with-parser-monad parser-m ~body))))
  ([name args body] `(defn ~name ~args (with-parser-monad parser-m ~body))))

;; Parser monad transformer
(defn parser-t
  ([m] (parser-t m :m-plus-from-transformer))
  ([m which-m-plus]
   (monad-transformer m which-m-plus
     [m-result (with-monad m
                 (fn [v]
                   (fn [st] (m-result [v st]))))
      m-bind   (with-monad m
                 (fn [p f]
                   (fn [st]
                     (let [ret (parse p st)]
                       (m-bind ret
                               (fn [[v out]]
                                 (parse (f v) out)))))))
      m-zero   (with-monad m (fn [st] m-zero))
      m-plus   (with-monad m
                 (fn [& ps]
                   (fn [st]
                     (apply m-plus (map #(parse % st) ps)))))
      ])))

;; Parser error monad
(defn- parser-error-ok [v] (list 'Right v))
(defn- parser-error-ng [v] (list 'Left v))
(defn- parser-error-status [m] (first m))
(defn- parser-error-value  [m] (second m))
(defn- parser-error-ok? [m] (= 'Right (parser-error-status m)))
(defn- parser-error-ng? [m] (not (parser-error-ok? m)))

(defmonad parser-error-m
  [m-result parser-error-ok
   m-bind   (fn [mv f]
              (if (parser-error-ok? mv)
                (f (parser-error-value mv))
                mv))
   m-zero   (parser-error-ng nil)
   m-plus   (fn [& mvs]
              (loop [mvs mvs]
                (if (and (parser-error-ng? (first mvs))
                         (next mvs))
                  (recur (next mvs))
                  (first mvs))))
   ])

;; with-monad for parser-monad
(defmacro with-parser-monad [monad & exprs]
  `(with-transformed-monad ~monad ~@exprs))

;; lift function
(defmacro parser-t$ [mv]
  `(fn [s#]
     (with-monad m-base
       (domonad [v# ~mv]
         [v# s#]))))

;; Parser monad
(def-transformed-monad parser-m parser-t parser-error-m)

;; Basic parser combinators
(with-parser-monad parser-m

  (defn fail [e] (fn [st] (parser-error-ng [e st])))
  (defn failure [e]
    (fn [st]
      (if (nil? fail)
        (with-monad m-base m-zero)
        ((fail e) st))))

  (defn item [st]
    (let [inp (:input st)
          [col line] (:pos st)]
      (if (= (count inp) 0)
        ((failure "No chars.") st)
        (let [next-char (su/get inp 0)
              rest-char (su/drop inp 1)
              new-pos   (if (= \newline next-char)
                          [0 (+ line 1)]
                          [(+ col 1) line])
              new-st    (assoc st :input rest-char :pos new-pos)]
          (with-monad m-base (m-result [next-char new-st]))))))

  (defn m-catch [mv msg]
    (m-plus mv
            (failure (str "Expected " msg))))

  (defn satisfy [pred]
    (domonad [x item
              :when (pred x)]
      x))

  (defn ch [x] (satisfy #(= x %)))

  (defn- digit?  [c] (Character/isDigit c))
  (defn- lower?  [c] (Character/isLowerCase c))
  (defn- upper?  [c] (Character/isUpperCase c))
  (defn- letter? [c] (Character/isLetter c))
  (defn- space?  [c] (Character/isWhitespace c))

  (def digit (satisfy digit?))
  (def lower (satisfy lower?))
  (def upper (satisfy upper?))
  (def letter (satisfy letter?))
  (def alpha-num (satisfy #(or (digit? %) (letter? %))))

  (declare many many1)
  (defn many [p]
    (m-plus (many1 p)
            (m-result nil)))
  (defn many1 [p]
    (domonad [v  p
              vs (many p)]
      (cons v vs)))

  (defn one-of [s]
    (domonad [x item
              :when (su/contains? s (str x))]
      x))
  (defn none-of [s]
    (domonad [x item
              :when (not (su/contains? s (str x)))]
      x))

  (declare sep-by sep-by1)
  (defn sep-by [p sep]
    (m-plus (sep-by1 p sep) (m-result nil)))
  (defn sep-by1 [p sep]
    (domonad [x  p
              xs (many (m-bind_ sep p))]
      (cons x xs)))

  (defn end-by [p sep]
    (many (domonad [x p
                    _ sep]
            x)))
  (defn end-by1 [p sep]
    (many1 (domonad [x p
                     _ sep]
             x)))

  (defn string [s]
    (m-catch
      (if (= 0 (count s))
        (m-result "")
        (domonad [_ (ch (su/get s 0))
                  _ (string (su/drop s 1))]
          s))
      (str "string : " s)))

  (defn skip-many [p]
    (domonad [_ (many p)] nil))

  (def spaces (skip-many (satisfy space?)))

  (def identifier
    (m-catch
      (domonad [x  lower
                xs (many alpha-num)]
        (apply str (cons x xs)))
      "identifier"))

  (def natural
    (m-catch
      (domonad [xs (many1 digit)]
        (Integer/parseInt (apply str xs)))
      "natural"))

  )
