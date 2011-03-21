(use 'parsure.core)

(defn make-json-atom [s] (list 'atom s))
(defn make-json-number [i] (list 'number i))
(defn make-json-string [s] (list 'string s))
(defn make-json-bool [b] (list 'bool b))
(defn make-json-list [l] (list 'list l))
(defn make-json-hash [h] (list 'hash h))

(declare parse-expr parse-symbol parse-atom parse-number
         parse-string parse-list parse-hash)

(defn parse-symbol [] (one-of "$"))

(defn parse-expr [] (doplus (parse-atom)
                            (parse-number)
                            (parse-string \')
                            (parse-string \")
                            (dobind [_ (ch \[)
                                     x (parse-list)
                                     _ (ch \])]
                                    (return x))
                            (dobind [_ (ch \{)
                                     x (parse-hash)
                                     _ (ch \})]
                                    (return x))))

(defn parse-atom [] (dobind [f (doplus letter (parse-symbol))
                             r (many (doplus letter digit (parse-symbol)))]
                            (let [at (apply str (cons f r))]
                              (cond (= at "true") (return (make-json-bool true))
                                    (= at "false") (return (make-json-bool false))
                                    :else (return (make-json-atom at))))))

(defn parse-number [] (lift make-json-number natural))

(defn parse-string [qt] (dobind [_ (ch qt)
                                 x (many (none-of (str qt)))
                                 _ (ch qt)]
                                (return (make-json-string (apply str x)))))

(defn parse-list [] (lift make-json-list
                          (sep-by (parse-expr) (dobind_ space (ch \,) space))))

(defn parse-pair [] (dobind [n (doplus (parse-string \') (parse-string \") (parse-atom))
                             _ (dobind_ space (ch \:) space)
                             v (parse-expr)]
                            (let [nm (nth n 1)]
                              (return (list nm v)))))

(defn parse-hash [] (lift make-json-hash
                          (sep-by (parse-pair) (dobind_ space (ch \,) space))))


(defn run [s]
  (parse-from-string (parse-expr) s))

(def sample-string
  "[{name: \"Taro\", age: 12}, {name: \"Jiro\", age: 11}]")
(run sample-string)

