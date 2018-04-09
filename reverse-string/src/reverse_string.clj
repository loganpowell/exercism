(ns reverse-string)

(defn reverse-string [& args] ;; <- arglist goes here
  (loop [coll (first args)
         acc  ""]
    (if (empty? coll)
        acc
        (recur (rest coll) (str (first coll) acc)))))
;; your code goes here

; test

; (reverse-string "Logan")
