(use 'parsure.core)
(require '[parsure.monad-ext :as m])

(defn make-csv-line      [l] (list 'Line l))
(defn make-csv-quoted    [s] (list 'Quoted s))
(defn make-csv-nonquoted [s] (list 'Non-quoted s))

(defparser parse-quoted-inner
  (m/do [xs (many (none-of "\""))
         xs2 (m/<|> (m/do [_ (string "\"\"")
                           xs parse-quoted-inner]
                          (str "\"" xs))
                  (m/return ""))]
        (str (apply str xs) xs2)))

(defparser parse-quoted
  (m/do [_ (ch \")
         x parse-quoted-inner
         _ (ch \")]
        (make-csv-quoted x)))

(defparser parse-nonquoted
  (m/fmap #(make-csv-nonquoted (apply str %))
          (many (none-of ",\n"))))

(defparser parse-line
  (m/fmap make-csv-line
          (sep-by1 (m/<|> parse-quoted parse-nonquoted) (ch \,))))

(defparser parse-csv
  (sep-by1 parse-line (ch \newline)))

(defn run [s] (parse-from-string parse-csv s))

(def sample-string "1,n0153,\"上司　和善\"\n2,n0154,\"山田 \"\"Razoku!!\"\" 大輔\"")
(run sample-string)
