(ns parsure.monad-ext
  (:use [clojure.contrib.monads]
        [clojure.contrib.macro-utils :only (defsymbolmacro)]))

;; Extends monad transform
(defmacro def-transformed-monad [name transformer base-m & exprs]
  `(let [~'m-base ~base-m]
     (do
       (def ~name (assoc (~transformer ~base-m) :m-base ~'m-base))
       ~@exprs
       )))

(declare m-bind_)
(defmacro with-transformed-monad [monad & exprs]
  `(let [~'m-base (:m-base ~monad)]
     (with-monad ~monad ~@exprs)))

(defsymbolmacro m-base m-base)

(defmonadfn m-bind_ [& steps]
  (reduce
    (fn [n p] (m-bind p (fn [_] n)))
    (reverse steps)))


;; Error monad
(defmonad error-m
  [m-result (fn [v] (list 'Right v))
   m-bind   (fn [mv f]
              (if (= 'Right (first mv))
                (f (second mv))
                mv))
   m-zero   (list 'Left nil)
   m-plus   (fn [& mvs]
              (defn iter [mvs]
                (if (= (count mvs) 1)
                  (first mvs)
                  (let [[lr v] (first mvs)]
                    (if (= 'Left lr)
                      (iter (rest mvs))
                      (list lr v)))))
              (iter mvs))
   ])

;; Error monad transformer
(defn error-t
  ([m] (error-t m :m-plus-default))
  ([m which-m-plus]
   (monad-transformer m which-m-plus
     [m-result (with-monad m
                 (fn [v] (m-result (list 'Right v))))
      m-bind   (with-monad m
                 (fn [mv f]
                   (m-bind mv
                           (fn [x]
                             (if (= 'Left (first x))
                               (m-result x)
                               (f (second x)))))))
      ])))
