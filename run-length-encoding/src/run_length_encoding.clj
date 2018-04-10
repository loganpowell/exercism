(ns run-length-encoding)




; PSEUDO
; 1)  process whole string into sequence
; 2) two levels of recursion:
; 2a) recurse over whole sequence to create a sequence of buckets
; put first into a bucket
; if second is same as first, put into same bucket if not put into new bucket
; 2b) for a bucket, count the number and create alias
; 3) conj all buckets into a single string


; (bucketer '(\a \a \a \b \b))
;
; (get (split-with (nth '(\a \a \a \b \b) 0)) '(\a \a \a \b \b)) 1
;
; ; THIS IS HANDY >> https://clojuredocs.org/clojure.core/partition-by
; (partition-by identity "AAAaaBBB  CCC")

(defn count-bucket
  [coll]
  (let [numr (count coll)
        alph (first coll)]
    (if (= 1 numr)
        (str alph)
        (str numr alph))))

; (count-bucket '(\a \a \a))

(defn run-length-encode
  "encodes a string with run-length-encoding"
  [& string]
  (let [list (seq (first string))]
    (if (empty? list)
        ""
        (let [bkts (partition-by identity list)]
          (loop [bucket bkts
                 acc ""]
            (if (empty? bucket)
                acc
                (do ;(println acc)
                    (recur (rest bucket) (str acc (count-bucket (first bucket)))))))))))

; (run-length-encode "aaaaaaaaaaaaaaa")

; (partition-by identity '(\1 \1 \w))



; DECODE ===


(defn decode-bucket
  [coll]
  (if (= 1 (count coll))
      (apply str coll)
      (let [bucket (split-with #(Character/isDigit %) coll)
            num (read-string (apply str (first bucket)))
            char (apply str (nth bucket 1))]
        (do ; (println bucket num char)
            (apply str (repeat num char))))))

; (decode-bucket '(\1 \2 \W))

; works!

(defn partitioner [list]
  (let [prior (atom \a)
        toggler (atom true)]
    (partition-by
      #(do
           ; (println list @prior %)
           (if (or
                 (Character/isLetter @prior)
                 (= \space @prior))
               (do (reset! prior %)
                   (reset! toggler (not @toggler)))
               (do (reset! prior %)
                   @toggler)))
           ; (do (println @toggler)
      list)))

; (map #(decode-bucket %) (partitioner '(\b \1 \2 \b \c)))
; (map (partial apply decode-bucket) '((\b) (\1 \2 \b) (\c)))


(defn run-length-decode
  "decodes a run-length-encoded string"
  [& string]
  (let [list (seq (first string))]
    (if (empty? list)
        ""
        (do ;(println string list)
            (let [partitions (partitioner list)]
              ; (println partitions)
              (apply str (map #(decode-bucket %) partitions)))))))

; (run-length-decode "12ab3fasdfasdb")





; ; NOPE
; (def decodering
;   (comp flatten (partial apply partition-by #(Character/isLetter %))))
;
; (decodering "12ab3b")
