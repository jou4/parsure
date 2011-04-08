(ns parsure.monad-ext
  (:use [clojure.contrib.monads]))

;; Monad function alias
(defmonadfn m-bind_ [& steps]
  (reduce
    (fn [n p] (m-bind p (fn [_] n)))
    (reverse steps)))

(defmacro do     [steps expr] `(domonad ~steps ~expr))
(defmacro >>=    [mv f] `(m-bind ~mv ~f))
(defmacro >>     [& mvs] `(m-bind_ ~@mvs))
(defmacro return [v] `(m-result ~v))
(defmacro fmap   [f m] `(m-fmap ~f ~m))
(defmacro lift-m [n f] `(m-lift ~n ~f))
(defmacro join   [m] `(m-join ~m))
