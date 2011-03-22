(use 'parsure.core :reload)

(defn make-json-atom [s] (list 'atom s))
(defn make-json-number [i] (list 'number i))
(defn make-json-string [s] (list 'string s))
(defn make-json-bool [b] (list 'bool b))
(defn make-json-list [l] (list 'list l))
(defn make-json-hash [h] (list 'hash h))

(declare parse-expr parse-symbol parse-atom parse-number
         parse-string parse-list parse-hash)

(defn trim [p] (dobind [_ spaces
                        x p
                        _ spaces]
                       (return x)))

(defparser parse-symbol (one-of "$"))

(defparser parse-expr
  (doplus parse-atom
          parse-number
          (parse-string \')
          (parse-string \")
          (dobind [_ (ch \[)
                   x parse-list
                   _ (ch \])]
                  (return x))
          (dobind [_ (ch \{)
                   x parse-hash
                   _ (ch \})]
                  (return x))
          (failure "Not matched any json-expression.")))

(defparser parse-atom
  (dobind [f (doplus letter parse-symbol)
           r (many (doplus letter digit parse-symbol))]
          (let [at (apply str (cons f r))]
            (cond (= at "true") (return (make-json-bool true))
                  (= at "false") (return (make-json-bool false))
                  :else (return (make-json-atom at))))))

(defparser parse-number
  (lift make-json-number natural))

(defn parse-string [qt]
  (defparser (dobind [_ (ch qt)
                      x (many (none-of (str qt)))
                      _ (ch qt)]
                     (return (make-json-string (apply str x))))))

(defparser parse-list
  (lift make-json-list
        (sep-by parse-expr (trim (ch \,)))))

(defparser parse-pair
  (dobind [n (doplus (parse-string \') (parse-string \") parse-atom)
           _ (trim (ch \:))
           v parse-expr]
          (let [nm (nth n 1)]
            (return (list nm v)))))

(defparser parse-hash
  (lift make-json-hash
        (sep-by parse-pair (trim (ch \,)))))


(defn run [s]
  (parse-from-string parse-expr s))

(def sample-string
  "[{name: \"Taro\", age: 12}, {name: \"Jiro\", age: 11}]")

(run sample-string)

