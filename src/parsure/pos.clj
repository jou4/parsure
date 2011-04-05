(ns parsure.pos)


(defrecord SourcePos [line column])

(defn new-pos [line column] (SourcePos. line column))
(defn initial-pos [] (SourcePos. 1 1))

(def source-line :line)
(def source-column :column)

(defn update-pos-char [^SourcePos pos c]
  (if (= c \newline)
    (SourcePos. (inc (source-line pos)) 1)
    (SourcePos. (source-line pos) (inc (source-column pos)))))

(defn show-pos [^SourcePos pos]
  (str "(line " (source-line pos) ", column " (source-column pos) ")"))
