(ns parsure.test.core
  (:use [clojure.test])
  (:use [parsure.core] :reload)
  (:require [parsure.monad-ext :as m]))

(defmacro p- [p s]
  `(clojure.contrib.monads/with-monad parser-m (parse ~p ~s)))
(defn check [ret pred]
  (pred ret))
(defn check= [ret expected]
  (check ret #(= (second %) expected)))
(defn checkf [ret]
  (check ret #(= 'Left (first %))))


(deftest test-primitive
  (is (check= (p- (m/return 1) "abcde") 1))
  (is (checkf (p- (unexpected "fail") "abcde")))
  (is (checkf (p- any-token "")))
  (is (check= (p- any-token "abcde") \a)))

(defparser p1 (m/do [x any-token
                     _ any-token
                     y any-token]
                (str x y)))
(defparser p2 (m/do [x m-zero
                     _ any-token
                     y any-token]
                (str x y)))
(defparser p3 (m/do [x (m/do [x any-token
                              _ any-token
                              y any-token]
                         (str x y))
                     y any-token]
                (str y x)))

(deftest test-monad
  (is (check= (p- p1 "abcde") "ac"))
  (is (checkf (p- p1 "ab")))
  (is (checkf (p- p2 "abcde")))
  (is (check= (p- p3 "abcde") "dac"))
  (is (check= (p- (<|> m-zero any-token) "abcde") \a))
  (is (check= (p- (<|> any-token m-zero) "abcde") \a))
  (is (checkf (p- (<|> m-zero m-zero) "abcde")))
  (is (check= (p- (<|> any-token (unexpected "fail")) "abcde") \a))
  (is (check= (p- (<|> (unexpected "fail") any-token) "abcde") \a))
  (is (checkf (p- (<|> (unexpected "fail") (unexpected "fail")) "abcde"))))

(defparser natural (m/fmap #(Integer/parseInt (apply str %))  (many1 digit)))
(defparser spaces (skip-many space))

(deftest test-parts
  (is (check= (p- (ch \a) "abc") \a))
  (is (checkf (p- (ch \a) "123")))
  (is (check= (p- digit "123") \1))
  (is (checkf (p- digit "abc")))
  (is (check= (p- lower "abc") \a))
  (is (checkf (p- lower "ABC")))
  (is (check= (p- upper "ABC") \A))
  (is (checkf (p- upper "abc")))
  (is (check= (p- letter "abc") \a))
  (is (check= (p- letter "ABC") \A))
  (is (checkf (p- letter "123")))
  (is (check= (p- alpha-num "abc") \a))
  (is (check= (p- alpha-num "123") \1))
  (is (checkf (p- alpha-num "[]")))
  (is (check= (p- (m/fmap #(apply str %) (many digit)) "123") "123"))
  (is (check= (p- (m/fmap #(apply str %) (many digit)) "abc") ""))
  (is (check= (p- (m/fmap #(apply str %) (many digit)) "") ""))
  (is (check= (p- (m/fmap #(apply str %) (many1 digit)) "123") "123"))
  (is (checkf (p- (m/fmap #(apply str %) (many1 digit)) "abc")))
  (is (checkf (p- (m/fmap #(apply str %) (many1 digit)) "")))
  (is (check= (p- (m/fmap #(apply str %) (many1 (one-of "abc"))) "cba123") "cba"))
  (is (checkf (p- (m/fmap #(apply str %) (many1 (one-of "abc"))) "123cba")))
  (is (check= (p- (m/fmap #(apply str %) (many1 (none-of "123"))) "cba123") "cba"))
  (is (checkf (p- (m/fmap #(apply str %) (many1 (none-of "123"))) "123cba")))
  (is (check= (p- (string "abc") "abcde") "abc"))
  (is (checkf (p- (string "abc") "12345")))
  (is (check= (p- natural "123abc") 123))
  (is (checkf (p- natural "abc123")))
  (is (check= (p- (m/>> spaces natural) "   123abc") 123)))

