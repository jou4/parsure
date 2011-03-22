(use 'parsure.core :reload)

(defn make-csv-line [l] (list 'line l))
(defn make-csv-quoted [s] (list 'quoted s))
(defn make-csv-nonquoted [s] (list 'nonquoted s))

(defparser parse-quoted-inner
  (dobind [xs (many (none-of "\""))
           xs2 (doplus (dobind [_ (string "\"\"")
                                xs parse-quoted-inner]
                               (return (str "\"" xs)))
                       (return ""))]
          (return (str (apply str xs) xs2))))

(defparser parse-quoted
  (dobind [_ (ch \")
           x parse-quoted-inner
           _ (ch \")]
          (return (make-csv-quoted x))))

(defparser parse-nonquoted
  (lift #(make-csv-nonquoted (apply str %))
        (many (none-of ",\n"))))

(defparser parse-line
  (lift make-csv-line
        (sep-by-1 (doplus parse-quoted parse-nonquoted) (ch \,))))

(defparser parse-csv
  (sep-by-1 parse-line (ch \newline)))


(defn run [s] (parse-from-string parse-csv s))

(def sample-string "1,n0153,\"Kamitsukasa Kazuyoshi\"\n2,n0154,\"Yamada \"\"Razoku!!\"\" Daisuke\"")
(run sample-string)

