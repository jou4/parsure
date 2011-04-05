(use 'parsure.core :reload)
(require '[parsure.monad-ext :as m])

(defn make-json-atom   [s] (list 'Atom s))
(defn make-json-number [i] (list 'Number i))
(defn make-json-string [s] (list 'String s))
(defn make-json-bool   [b] (list 'Bool b))
(defn make-json-list   [l] (list 'List l))
(defn make-json-hash   [h] (list 'Hash h))

(declare parse-expr parse-symbol parse-atom parse-number
         parse-string parse-list parse-hash)


(defparser spaces (skip-many space))
(defparser natural (many1 digit))

(defparser trim [p] (m/do [_ spaces
                           x p
                           _ spaces]
                          x))

(defparser parse-symbol (one-of "$"))

(defparser parse-atom
  (m/do [f (<|> letter parse-symbol)
         r (many (<|> letter digit parse-symbol))]
        (let [at (apply str (cons f r))]
          (cond (= at "true") (make-json-bool true)
                (= at "false") (make-json-bool false)
                :else (make-json-atom at)))))

(defparser parse-number
  (m/fmap make-json-number natural))

(defparser parse-string [qt]
  (m/do [_ (ch qt)
         x (many (none-of (str qt)))
         _ (ch qt)]
        (make-json-string (apply str x))))

(defparser parse-list
  (m/fmap make-json-list
          (sep-by parse-expr (trim (ch \,)))))

(defparser parse-pair
  (m/do [n (<|> (parse-string \') (parse-string \") parse-atom)
         _ (trim (ch \:))
         v parse-expr]
        (let [nm (nth n 1)]
          (list nm v))))

(defparser parse-hash
  (m/fmap make-json-hash
          (sep-by parse-pair (trim (ch \,)))))

(defparser parse-expr
  (<|> parse-atom
     parse-number
     (parse-string \')
     (parse-string \")
     (m/do [_ (ch \[)
            x parse-list
            _ (ch \])]
           x)
     (m/do [_ (ch \{)
            x parse-hash
            _ (ch \})]
           x)))

(defn run [s]
  (let [[lr ret] (parse parse-expr s)]
    (if (= 'Left lr) (show ret) ret)))

(def sample-string
  "[{name: \"Taro\", age: 12}, {name: \"Jiro\", age: 11}]")

(run sample-string)
