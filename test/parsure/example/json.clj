(ns parsure.example.json
  (:refer-clojure :exclude [char count])
  (:use [parsure core char combinator] :reload)
  (:require [parsure.monad-ext :as m]))


(defn make-json-atom   [s] (list 'Atom s))
(defn make-json-number [i] (list 'Number i))
(defn make-json-string [s] (list 'String s))
(defn make-json-bool   [b] (list 'Bool b))
(defn make-json-list   [l] (list 'List l))
(defn make-json-hash   [h] (list 'Hash h))


(declare parse-expr parse-symbol parse-atom parse-number
         parse-string parse-list parse-hash)


(defparser spaces (skip-many (satisfy #(Character/isWhitespace %))))
(defparser natural (m/fmap #(Integer/parseInt (apply str %)) (many1 digit)))

(defparser trim [p] (m/do [_ spaces
                           x p
                           _ spaces]
                      x))

(defparser parse-symbol (one-of "$"))

(defparser parse-atom
  (m/do [f (choice letter parse-symbol)
         r (many (choice letter digit parse-symbol))]
    (let [at (apply str (cons f r))]
      (cond (= at "true") (make-json-bool true)
            (= at "false") (make-json-bool false)
            :else (make-json-atom at)))))

(defparser parse-number
  (m/fmap make-json-number (trim natural)))

(defparser parse-string [qt]
  (trim (m/do [_ (char qt)
               x (many (none-of (str qt)))
               _ (char qt)]
          (make-json-string (apply str x)))))

(defparser parse-list
  (trim (m/fmap make-json-list
                (sep-by parse-expr (trim (char \,))))))

(defparser parse-pair
  (trim (m/do [n (choice (parse-string \') (parse-string \") parse-atom)
               _ (trim (char \:))
               v parse-expr]
          (let [nm (nth n 1)]
            (list nm v)))))

(defparser parse-hash
  (trim (m/fmap make-json-hash
                (sep-by parse-pair (trim (char \,))))))

(defparser parse-expr
  (choice parse-atom
          parse-number
          (parse-string \')
          (parse-string \")
          (m/do [_ (char \[)
                 x parse-list
                 _ (char \])]
            x)
          (m/do [_ (char \{)
                 x parse-hash
                 _ (char \})]
            x)))

(defn run [s]
  (let [[lr ret] (parse parse-expr s)]
    (if (= 'Left lr) (show ret) ret)))

(def sample-string
  "[{name: \"Taro\", age: 12}, {name: \"Jiro\", age: 11}]")

(run sample-string)
