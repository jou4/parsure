(ns parsure.combinator
  (:refer-clojure :exclude [count])
  (:use [parsure.core])
  (:require [clojure.contrib.monads :as m]))

(m/with-monad parser-m

  (defn choice [& ps]
    (reduce (fn [p q] (m-plus q p)) m-zero (reverse ps)))

  (defn option [x p]
    (<|> p (m-result x)))

  (defn between [open close p]
    (m/domonad [_ open
                x p
                _ close]
      x))

  (defn skip-many [p]
    (m/domonad [_ (many-accum cons p)] nil))

  (defn skip-many1 [p]
    (m/domonad [_ p
                _ (skip-many p)]
      nil))

  (defn many [p]
    (m/domonad [xs (many-accum cons p)] (reverse xs)))

  (defn many1 [p]
    (m/domonad [x  p
                xs (many p)]
      (cons x xs)))

  (declare sep-by sep-by1)

  (defn sep-by [p sep]
    (m-plus (sep-by1 p sep) (m-result [])))

  (defn sep-by1 [p sep]
    (m/domonad [x  p
                xs (many (m/domonad [_  sep xs p] xs))]
      (cons x xs)))

  (declare sep-end-by sep-end-by1)

  (defn sep-end-by [p sep]
    (<|> (sep-end-by1 p sep) (m-result nil)))

  (defn sep-end-by1 [p sep]
    (m-bind p
            (fn [x] (<|> (m/domonad [_  sep
                                     xs (sep-end-by p sep)]
                           (cons x xs))
                       (m-result [x])))))

  (defn end-by [p sep]
    (many (m/domonad [x p
                      _ sep]
            x)))

  (defn end-by1 [p sep]
    (many1 (m/domonad [x p
                       _ sep]
             x)))

  (defn count [n p]
    (if (<= n 0)
      (m-result nil)
      (m/m-seq (replicate n p))))

  (def any-token
    (token-prim show
                (fn [pos c cs] pos)
                (fn [c] true)))

  (def eof (<?> (not-followed-by any-token) "end of input"))

  (defn many-till [p end]
    (<|> (m/domonad [_ end] nil)
       (m/domonad [x  p
                   xs (many-till p end)]
         (cons x xs))))

  (defn look-ahead [p]
    (m/domonad [st (get-parser-state)
                x  p
                _  (set-parser-state st)]
      x))

  )
