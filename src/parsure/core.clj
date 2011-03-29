(ns parsure.core
  (:use [clojure.contrib.monads]
        [parsure.monad-ext])
  (:require [clojure.contrib.str-utils2 :as su]))

;; Do parse
(defn parse [p inp] ((force p) inp))
(defn parse-from-string [p inp] (parse p inp))

;; Define parser combinator
(defmacro defparser
  ([name body] `(def ~name (delay ~body)))
  ([name args body] `(defn ~name ~args ~body)))

;; Parser monad transformer
(defn parser-t
  ([m] (parser-t m :m-plus-from-transformer))
  ([m which-m-plus]
   (monad-transformer m which-m-plus
     [m-result (with-monad m
                 (fn [v]
                   (fn [inp] (m-result [v inp]))))
      m-bind   (with-monad m
                 (fn [p f]
                   (fn [inp]
                     (let [ret (parse p inp)]
                       (m-bind ret
                               (fn [[v out]]
                                 (parse (f v) out)))))))
      m-zero   (with-monad m (fn [inp] m-zero))
      m-plus   (with-monad m
                 (fn [& ps]
                   (fn [inp]
                     (apply m-plus (map #(parse % inp) ps)))))
      ])))

;; with-monad for parser-monad
(defmacro with-parser-monad [monad & exprs]
  `(with-transformed-monad ~monad ~@exprs))

;; lift function
(defmacro parser-t$ [mv]
  `(fn [s#]
     (with-monad m-base
       (domonad [v# ~mv]
         [v# s#]))))

;; Build parser
(defmacro build-parser [name transformer base-m & exprs]
  `(def-transformed-monad
     ~name ~transformer ~base-m (attach-parsers ~name) ~@exprs))

(declare basic-parser-combinators)
(defmacro attach-parsers [monad]
  `(with-parser-monad ~monad
     ~@basic-parser-combinators))

;; Basic parser combinators
(def basic-parser-combinators
  '(

    (defn fail [e] (fn [st] nil))
    (defn failure [e]
      (fn [st]
        (if (nil? fail)
          (with-monad m-base m-zero)
          ((fail e) st))))

    (defn item [inp]
      (if (= (count inp) 0)
        ((failure "No chars.") inp)
        (with-monad m-base (m-result [(su/get inp 0) (su/drop inp 1)]))))

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

    ))

;; Default parser monad
(build-parser parser-m parser-t maybe-m)
