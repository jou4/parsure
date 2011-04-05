(ns parsure.error
  (:use [parsure.pos]))


(defrecord ParseError [pos msgs])

(defn- from-enum [[err-type _]]
  (cond (= err-type 'SysUnExpect) 0
        (= err-type 'UnExpect)    1
        (= err-type 'Expect)      2
        :else                     3))

(def error-pos :pos)
(defn error-messages [e] (sort (fn [a b] (compare (from-enum a) (from-enum b))) (:msgs e)))

(def message-string second)

(defn error-is-unknown [{:keys [_ msgs]}] (empty? msgs))

(defn new-error-unknown [pos] (ParseError. pos []))

(defn new-error-message [msg pos] (ParseError. pos [msg]))

(defn add-error-message [msg {:keys [pos msgs]}]
  (ParseError. pos (cons msg msgs)))

(defn set-error-pos [pos {:keys [_ msgs]}] (ParseError. pos msgs))

(defn set-error-message [msg {:keys [pos msgs]}]
  (ParseError. pos
               (cons msg
                     (filter #(not= msg %) msgs))))

(defn merge-error [err1 err2]
  (ParseError. (:pos err1) (concat (:msgs err1) (:msgs err2))))

(defn show-error [^ParseError e]
  (let [msgs (error-messages e)
        [sys-unexpect msgs1] (split-with #(= 'SysUnExpect (first %)) msgs)
        [unexpect     msgs2] (split-with #(= 'UnExpect (first %)) msgs1)
        [expect       msgs3] (split-with #(= 'Expect (first %)) msgs2)
        comma-sep (fn [a] (apply str (interpose ", " a)))
        commas-or (fn [ms]
                    (cond (empty? ms) ""
                          (= (count ms) 1) (first ms)
                          :else (str (comma-sep (butlast ms)) " or " (last ms))))
        show-many (fn [pre msgs]
                    (let [ms (distinct (map message-string msgs))]
                      (if (empty? ms)
                        ""
                        (if (empty? pre)
                          (commas-or ms)
                          (str pre " " (commas-or ms))))))
        show-expect (show-many "expecting" expect)
        show-unexpect (show-many "unexpected" unexpect)
        show-sys-unexpect (cond (or (not (empty? unexpect)) (empty? sys-unexpect)) ""
                                (empty? (message-string (first sys-unexpect))) "unexpected end of input"
                                :else   (str "unexpected " (message-string (first sys-unexpect))))
        show-messages (show-many "" msgs3)
        ]
    (println (str (show-pos (:pos e))
                  " : \n - "
                  (if (empty? msgs)
                    "unknown parser error"
                    (apply str (interpose "\n - " (filter #(not (empty? %)) [show-expect show-unexpect show-sys-unexpect show-messages]))))))))
