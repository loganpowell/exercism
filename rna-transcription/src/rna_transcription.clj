(ns rna-transcription)

(defn rna-translator [coll]
  (loop [temp coll
         acc '[]]
    (if (= empty? temp)
      acc
      (recur
        (rest temp)
        (concat
          (case (first temp)
            \G \C
            \C \G
            \T \A
            \A \U
            "BRRT") 
          acc)))))

(rna-translator \G)

(defn to-rna [string] ;; <- arglist goes here
  (reduce))
  ;; your code goes here
(defn test-1
  [coll]
  (let [temp coll]
    (case (first temp)
          \G \C
          \C \G
          \T \A
          \A \U)))

(test-1 '(\T))
