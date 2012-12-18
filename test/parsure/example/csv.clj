(ns parsure.example.csv
  (:refer-clojure :exclude [char count])
  (:use [parsure core char combinator] :reload)
  (:require [parsure.monad-ext :as m]))

(defn make-csv-line      [l] (list 'Line l))
(defn make-csv-quoted    [s] (list 'Quoted s))
(defn make-csv-nonquoted [s] (list 'Non-quoted s))


(defparser parse-quoted-inner
  (m/do [xs (many (none-of "\""))
         xs2 (<|> (m/do [_ (string "\"\"")
                         xs parse-quoted-inner]
                    (str "\"" xs))
                (m/return ""))]
    (str (apply str xs) xs2)))

(defparser parse-quoted
  (m/do [_ (char \")
         x parse-quoted-inner
         _ (char \")]
    (make-csv-quoted x)))

(defparser parse-nonquoted
  (m/fmap #(make-csv-nonquoted (apply str %))
          (many (none-of ",\n"))))

(defparser parse-line
  (m/fmap make-csv-line
          (sep-by1 (<|> parse-quoted parse-nonquoted) (char \,))))

(defparser parse-csv
  (sep-by1 parse-line (char \newline)))

(defmacro run [s & opts]
  `(let [[lr# ret#] (parse-from-file parse-csv ~s ~@opts)]
    (if (= 'Left lr#) (show ret#) ret#)))

(run "test/parsure/example/utf8.csv" :encoding "UTF-8")
(run "test/parsure/example/sjis.csv" :encoding "Shift_JIS")
