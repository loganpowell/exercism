(ns rna-transcription)



(defn trans-each
  [char]
  (let [each (case char
               \G \C
               \C \G
               \T \A
               \A \U
               "NA")]
    (if (= each "NA")
      (throw (AssertionError. "Unknown input."))
      each)))

(defn to-rna [string] ;; <- arglist goes here
  (let [coll (seq string)]
    (loop [left coll
           acc ""]
      (if (empty? left)
        acc
        (recur (rest left)
               (str acc (trans-each (first left))))))))


  ;; your code goes here

; (to-rna "X")
;
; (time (test-1 '(\T)))
