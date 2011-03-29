(use 'parsure.core)
(require '[clojure.contrib.monads :as m])

(defn make-json-atom   [s] (list 'Atom s))
(defn make-json-number [i] (list 'Number i))
(defn make-json-string [s] (list 'String s))
(defn make-json-bool   [b] (list 'Bool b))
(defn make-json-list   [l] (list 'List l))
(defn make-json-hash   [h] (list 'Hash h))

(declare parse-expr parse-symbol parse-atom parse-number
         parse-string parse-list parse-hash)

(with-parser-monad parser-m
  (defparser trim [p] (m/domonad [_ spaces
                                  x p
                                  _ spaces]
                                 x))

  (defparser parse-symbol (one-of "$"))

  (defparser parse-atom
    (m/domonad [f (m-plus letter parse-symbol)
                r (many (m-plus letter digit parse-symbol))]
               (let [at (apply str (cons f r))]
                 (cond (= at "true") (make-json-bool true)
                       (= at "false") (make-json-bool false)
                       :else (make-json-atom at)))))

  (defparser parse-number
    (m/m-fmap make-json-number natural))

  (defparser parse-string [qt]
    (m/domonad [_ (ch qt)
                x (many (none-of (str qt)))
                _ (ch qt)]
               (make-json-string (apply str x))))

  (defparser parse-list
    (m/m-fmap make-json-list
              (sep-by parse-expr (trim (ch \,)))))

  (defparser parse-pair
    (m/domonad [n (m-plus (parse-string \') (parse-string \") parse-atom)
                _ (trim (ch \:))
                v parse-expr]
               (let [nm (nth n 1)]
                 (list nm v))))

  (defparser parse-hash
    (m/m-fmap make-json-hash
              (sep-by parse-pair (trim (ch \,)))))

  (defparser parse-expr
    (m-plus parse-atom
            parse-number
            (parse-string \')
            (parse-string \")
            (m/domonad [_ (ch \[)
                        x parse-list
                        _ (ch \])]
                       x)
            (m/domonad [_ (ch \{)
                        x parse-hash
                        _ (ch \})]
                       x)
            (fail "Not matched any json-expression.")))
  )

(defn run [s]
  (parse-from-string parse-expr s))

(def sample-string
  "[{name: \"Taro\", age: 12}, {name: \"Jiro\", age: 11}]")

(run sample-string)
