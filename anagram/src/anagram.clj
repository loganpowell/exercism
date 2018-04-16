(ns anagram
  (:use [clojure.string :only [lower-case]] :reload))


; pseudo
; 1) digesting input: Vector (list of words)
; create key : value pairing of words with their chars as a list
; sort list of chars alphabetically for each key
; 2) digesting input: word
; turn word into sequence and sort <- create reusable function
; 3) compare each group (key: list) list to word (= group word)
; if group = word: conj key into '[]
; when done with the comparison (vector input is exhausted) return []


(defn word-chars-map [words-vector]
  (let
    [map-list
     (for [word words-vector
           :let [map {word (sort (seq (lower-case word)))}]]
          map)]
    (into {} map-list)))
; into: https://clojuredocs.org/clojure.core/for#example-542692d3c026201cdc326faa

; (word-chars-map ["gallery" "BALLERINA" "regally" "clergy" "largely" "leading"])

; (word-chars-map ["cashregister" "Carthorse" "radishes"])b

(defn anagrams-for [word words-vec] ;; <- arglist goes here
  (let [words-map (word-chars-map words-vec)
        word-chars (sort (seq (lower-case word)))]
    (for [[key val] words-map :when
          (and
            (= val word-chars)
            (not= key (lower-case word)))]
      key)))

; (anagrams-for "BANANA" ["banana"])
; (anagrams-for "allergy" ["gallery" "ballerina" "regally" "clergy" "largely" "leading"])

; (anagrams-for "Orchestra" ["cashregister" "Carthorse" "radishes"])
