(ns parsure.example.expr
  (:use [parsure core [combinator :exclude [count]]] :reload)
  (:require [parsure [monad-ext :as m] [char :as ch]]))


(declare expr term factor mulop addop)

(defparser expr (chainl1 term addop))

(defparser term (chainl1 factor mulop))

(defparser factor
  (<|> (between (ch/char \() (ch/char \)) expr)
     (m/fmap #(Integer/parseInt (apply str %)) (many1 ch/digit))))

(defparser mulop
  (<|> (m/do [_ (ch/char \*)]
         *)
     (m/do [_ (ch/char \/)]
       /)))
(defparser addop
  (<|> (m/do [_ (ch/char \+)]
         +)
     (m/do [_ (ch/char \-)]
       -)))

(parse expr "3*(1+2+3)/2-6")
