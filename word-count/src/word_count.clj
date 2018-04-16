(ns word-count)


; Pseudo
; parts:
; I. content digestion
; 1) content as input
; 3) filter out anything that's not Character/isLetterOrNumber or " " (\space)
; 2) break content into words (partition-by " ")
; 4) all words to lowercase)

; II. word accumulator
; 1) for each word in sequence:
; 2a) if not seen before, create a new key in map {"key" count}
; 2b) if in table, inc the 'count' (value) for that key

; FIRST TRANSDUCERS!!!

(def chars->words
  (comp
    (filter #(or (= \space %) (Character/isLetterOrDigit %)))
    (partition-by #(= \space %))
    (map #(apply str %))
    (map #(clojure.string/lower-case %))
    (remove #(or (= " " %) (= "  " %))))) ; punting on double space failures

(defn str->words [string]
  (let [chars (seq string)] ; convert to coll for transducers
    (transduce chars->words conj '[] chars)))

; (str->words "My ass is in, the swamp! 1 2 4")


; More Pseudo
; 1) take string and break into words
; 2) for each word:
; if it's recorded in state, increment that record and move to the next/rest
; if it's not recorded, conj it into the state and next/rest
; base case = when list is empty return state
; can we use a conditional `recur`? Let's try that first
; if that doesn't work, we might need an atom (yep, went with atom)

; help: https://clojuredocs.org/clojure.core/swap!#example-564a7502e4b0be225c0c4794

(defn chk-wrd-db [word atom]
  (if (get (deref atom) word); get word out of returned map
      (swap! atom update-in [word] inc)
      (swap! atom conj {word 1}))) ; to conj into {atom}, wrap in map

; (get (deref (atom {"ass" 2})) "ass")

; (chk-wrd-db "butts" (atom {"ass" 2}))


(defn word-count [string] ;; <- arglist goes here
  (let [words (str->words string)
        db (atom {})] ; stateful function
    (loop [set words
           acc db]
      (if
        (empty? set)
        acc
        (recur (rest set) (chk-wrd-db (first set) db))))))

; (word-count "car : carpet as java : javascript!!&@$%^&")
  ;; your code goes here
