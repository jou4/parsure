(use 'parsure.core)

(defn make-json-atom [s] (list 'atom s))
(defn make-json-number [i] (list 'number i))
(defn make-json-string [s] (list 'string s))
(defn make-json-bool [b] (list 'bool b))
(defn make-json-list [l] (list 'list l))
(defn make-json-hash [h] (list 'hash h))

(declare parse-expr parse-symbol parse-atom parse-number
         parse-string parse-list parse-hash)

(def parse-symbol (defparser (one-of "$")))

(def parse-expr
  (defparser (doplus parse-atom
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
                     (failure "Not matched any json-expression."))))

(def parse-atom
  (defparser (dobind [f (doplus letter parse-symbol)
                      r (many (doplus letter digit parse-symbol))]
                     (let [at (apply str (cons f r))]
                       (cond (= at "true") (return (make-json-bool true))
                             (= at "false") (return (make-json-bool false))
                             :else (return (make-json-atom at)))))))

(def parse-number
  (defparser (lift make-json-number natural)))

(defn parse-string [qt]
  (defparser (dobind [_ (ch qt)
                      x (many (none-of (str qt)))
                      _ (ch qt)]
                     (return (make-json-string (apply str x))))))

(def parse-list
  (defparser (lift make-json-list
                   (sep-by parse-expr (dobind_ space (ch \,) space)))))

(def parse-pair
  (defparser (dobind [n (doplus (parse-string \') (parse-string \") parse-atom)
                      _ (dobind_ space (ch \:) space)
                      v parse-expr]
                     (let [nm (nth n 1)]
                       (return (list nm v))))))

(def parse-hash
  (defparser (lift make-json-hash
                   (sep-by parse-pair (dobind_ space (ch \,) space)))))


(defn run [s]
  (parse-from-string parse-expr s))

(def sample-string
  "[{name: \"Taro\", age: 12}, {name: \"Jiro\", age: 11}]")

(run sample-string)

