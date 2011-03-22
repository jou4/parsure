(ns parsure.test.core
  (:use [parsure.core] :reload)
  (:use [clojure.test]))

(defn p- [p s]
  (parse-from-string p s))
(defn assert-val [ret expected]
  (= (get-value ret) expected))
(defn assert-fail [ret]
  (failed? ret))

(deftest test-parse-01
  (is (assert-val (p- (return 1) "abc") 1))
  (is (assert-fail (p- (failure) "abc")))
  (is (assert-fail (p- item "")))
  (is (assert-val (p- item "abc") \a))
  )

(def p1 (dobind [x item
                 _ item
                 y item]
                (return (list x y))))
(def p2 (dobind [x (dobind [x item
                            _ item
                            y item]
                           (return (str x y)))
                 y item]
                (return (str y x))))

(deftest test-parse-02
  (is (assert-val (p- p1 "abcdef") '(\a \c)))
  (is (assert-fail (p- p1 "ab")))
  (is (assert-val (p- p2 "abcdef") "dac"))
  )

(def p3 (doplus (failure)
                (failure)
                (return \d)))

(deftest test-parse-03
  (is (assert-val (p- (plus item (return \d)) "abc") \a))
  (is (assert-val (p- (plus (failure) (return \d)) "abc") \d))
  (is (assert-fail (p- (plus (failure) (failure)) "abc")))
  (is (assert-val (p- p3 "abc") \d))
  )

(deftest test-parser-components-04
  (is (assert-val (p- digit "123") \1))
  (is (assert-fail (p- digit "abc")))
  (is (assert-val (p- (ch \a) "abc") \a))
  (is (assert-fail (p- (ch \a) "123")))
  (is (assert-val (p- (string "abc") "abcdef") "abc"))
  (is (assert-fail (p- (string "abc") "ab123")))
  (is (assert-val (p- (many digit) "123abc") '(\1 \2 \3)))
  (is (assert-val (p- (many digit) "abcdef") nil))
  (is (assert-fail (p- (many1 digit) "abcdef")))
  (is (assert-val (p- ident "abc def") "abc"))
  (is (assert-val (p- nat "123 abc") 123))
  (is (assert-val (p- spaces "  abc") nil))
  )

(def p4
  (dobind [_ (sym "[")
           n natural
           n2 (many (dobind [_ (sym ",")]
                            natural))
           _ (sym "]")]
          (return (cons n n2))))

(deftest test-parse-components-05
  (is (assert-val (p- p4 "[ 1, 2, 3 ]") '(1 2 3)))
  (is (assert-fail (p- p4 "[ 1, 2, ]")))
  )


