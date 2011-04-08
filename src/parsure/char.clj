(ns parsure.char
  (:refer-clojure :exclude [char])
  (:use [parsure core pos])
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
  (defn- lower?  [c] (Character/isLowerCase c))
  (defn- upper?  [c] (Character/isUpperCase c))
  (defn- letter? [c] (Character/isLetter c))
  (defn- space?  [c] (Character/isSpace c))

  (def digit     (<?> (satisfy digit?) "digit"))
  (def lower     (<?> (satisfy lower?) "lowercase letter"))
  (def upper     (<?> (satisfy upper?) "uppercase letter"))
  (def letter    (<?> (satisfy letter?) "letter"))
  (def space     (<?> (satisfy space?) "space"))
  (def alpha-num (<?> (satisfy #(or (digit? %) (letter? %))) "letter or digit"))

  (defn string [s]
    (if (= 0 (clojure.core/count s))
      (m-result "")
      (parsure.core/try
        (m/domonad [_ (char (first s))
                    _ (string (rest s))]
          s))))

  )
