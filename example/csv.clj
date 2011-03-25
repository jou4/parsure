(use 'clojure.contrib.monads)
(use 'parsure.core :reload)
(use 'parsure.monad-ext :reload)

(defn make-csv-line      [l] (list 'Line l))
(defn make-csv-quoted    [s] (list 'Quoted s))
(defn make-csv-nonquoted [s] (list 'Non-quoted s))

(with-parser parser-m
  (defparser parse-quoted-inner
    (domonad [xs (many (none-of "\""))
              xs2 (m-plus (domonad [_ (string "\"\"")
                                    xs parse-quoted-inner]
                            (str "\"" xs))
                          (m-result ""))]
      (str (apply str xs) xs2)))

  (defparser parse-quoted
    (domonad [_ (ch \")
              x parse-quoted-inner
              _ (ch \")]
      (make-csv-quoted x)))

  (defparser parse-nonquoted
    (m-fmap #(make-csv-nonquoted (apply str %))
            (many (none-of ",\n"))))

  (defparser parse-line
    (m-fmap make-csv-line
            (sep-by1 (m-plus parse-quoted parse-nonquoted) (ch \,))))

  (defparser parse-csv
    (sep-by1 parse-line (ch \newline)))
  )

(defn run [s] (parse-from-string parse-csv s))

(def sample-string "1,n0153,\"Kamitsukasa Kazuyoshi\"\n2,n0154,\"Yamada \"\"Razoku!!\"\" Daisuke\"")
(run sample-string)
