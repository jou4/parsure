(ns parsure.char)

(defn digit? [c]
  (Character/isDigit c))
(defn lower? [c]
  (Character/isLowerCase c))
(defn upper? [c]
  (Character/isUpperCase c))
(defn letter? [c]
  (Character/isLetter c))
