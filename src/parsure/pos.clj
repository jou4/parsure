(ns parsure.pos
  (:refer-clojure :exclude [name]))


(defrecord SourcePos [name line column])

(defn new-pos [name line column] (SourcePos. name line column))
(defn initial-pos [name] (SourcePos. name 1 1))

(def source-name :name)
(def source-line :line)
(def source-column :column)

(defn update-pos-char [^SourcePos pos c]
  (if (= c \newline)
    (SourcePos. (source-name pos) (inc (source-line pos)) 1)
    (SourcePos. (source-name pos) (source-line pos) (inc (source-column pos)))))

(defn show-pos [^SourcePos pos]
  (let [name (source-name pos)]
    (str (if (empty? name)
           ""
           (str "\"" name "\" "))
         (str "(line " (source-line pos) ", column " (source-column pos) ")"))))
