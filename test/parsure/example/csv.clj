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

(defn run [s]
  (let [[lr ret] (parse parse-csv s)]
    (if (= 'Left lr) (show ret) ret)))

(def sample-string "1,n0153,\"鈴木　一郎\"\n2,n0154,\"山田 \"\"Keyton\"\" 大輔\"")
(run sample-string)
