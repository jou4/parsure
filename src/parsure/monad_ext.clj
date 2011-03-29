(ns parsure.monad-ext
  (:refer-clojure :exclude [do])
  (:use [clojure.contrib.monads]
        [clojure.contrib.macro-utils :only (defsymbolmacro)]))

;; Extends monad transform
(defmacro def-transformed-monad [name transformer base-m & exprs]
  `(let [~'m-base ~base-m]
     (do
       (def ~name (assoc (~transformer ~base-m) :m-base ~'m-base))
       ~@exprs
       )))

(defmacro with-transformed-monad [monad & exprs]
  `(let [~'m-base (:m-base ~monad)]
     (with-monad ~monad ~@exprs)))

(defsymbolmacro m-base m-base)

(defmonadfn m-bind_ [& steps]
  (reduce
    (fn [n p] (m-bind p (fn [_] n)))
    (reverse steps)))


;; Monad function alias
(defmacro do [steps expr] `(domonad ~steps ~expr))
(defmacro >>= [mv f] `(m-bind ~mv ~f))
(defmacro >> [& mvs] `(m-bind_ ~@mvs))
(defmacro return [v] `(m-result ~v))
(defmacro <|> [& mvs] `(m-plus ~@mvs))
(defmacro fmap [f m] `(m-fmap ~f ~m))
(defmacro lift-m [n f] `(m-lift ~n ~f))
(defmacro join [m] `(m-join ~m))
