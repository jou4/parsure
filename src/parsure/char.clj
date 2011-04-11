(ns parsure.char
  (:refer-clojure :exclude [char])
  (:use [parsure core pos [combinator :only [skip-many]]])
  (:require [clojure.contrib.monads :as m]))

(m/with-monad parser-m

  (defn satisfy [pred]
    (token-prim show
                (fn [pos c cs] (update-pos-char pos c))
                (fn [c] (if (pred c) c nil))))

  (defn one-of  [s] (satisfy #((set s) %)))

  (defn none-of [s] (satisfy #(nil? ((set s) %))))

  (defn char [c] (<?> (satisfy #(= c %)) (show c)))

  (def any-char (satisfy (fn [_] true)))

  (defn- digit?  [c] (Character/isDigit c))
  (defn- hex-digit? [c]
    (if (digit? c)
      true
      (let [code (int c)]
        (or (and (>= code 97) (<= code 102))
            (and (>= code 65) (<= code 70))))))
  (defn- oct-digit? [c]
    (let [code (int c)]
      (and (>= code 48) (<= code 55))))

  (defn- lower?  [c] (Character/isLowerCase c))
  (defn- upper?  [c] (Character/isUpperCase c))
  (defn- letter? [c] (Character/isLetter c))
  (defn- space?  [c] (Character/isSpace c))

  (def digit     (<?> (satisfy digit?) "digit"))
  (def hex-digit (<?> (satisfy hex-digit?) "hexadeximal digit"))
  (def oct-digit (<?> (satisfy oct-digit?) "octal digit"))
  (def lower     (<?> (satisfy lower?) "lowercase letter"))
  (def upper     (<?> (satisfy upper?) "uppercase letter"))
  (def letter    (<?> (satisfy letter?) "letter"))
  (def alpha-num (<?> (satisfy #(or (digit? %) (letter? %))) "letter or digit"))
  (def space     (<?> (satisfy space?) "space"))
  (def spaces    (<?> (skip-many space) "white space"))

  (defn string [s]
    (if (= 0 (clojure.core/count s))
      (m-result "")
      (parsure.core/try
        (m/domonad [_ (char (first s))
                    _ (string (rest s))]
          s))))

  )
