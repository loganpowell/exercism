(ns isbn-verifier)
; PSEUDO

; part 1
; break string into sequence
; remove dashes count chars (= (count chars) 10)

; part 2a
; first 9 chars must be `Character/isDigit`
; multiply (using `for`) the first 9 numbers by the range from 2 - 10
; with 10 being multiplied by the first and 2 by the 9th
; get the sum of the products for the first 9 (call this z)

; part 2b
; char must be (or (Character/isDigit %) (= \X %))
; if `check-digit` (y) = X add 10 to z
; else add `check-digit` to z (`total`)

; part 3
; get `mod` of `total`/11
; if mod = 0 true | else false

(defn first-9 [seq]
  (if
    (every? #(Character/isDigit %) seq)
    (let [vects (map vector (reverse (range 2 11)) (map #(Character/digit % 10) seq))]
      (let [products (map #(apply * %) vects)]
        (apply + products)))
    1))

; help 1)

(time (first-9 '(\1 \2 \3 \4 \5 \6 \7 \8 \9)))

(defn str-2-seq [str]
  (let [chars (filter #(not= \- %) (seq str))]
    chars
    (if (= 10 (count chars))
        (let [digits (first (split-at 9 chars))
              checkr (second (split-at 9 chars))]
          ; (println "digits: " digits " checkr: " checkr))
          [digits checkr])
        '[(\1 \1 \1 \1 \1 \1 \1 \1 \0) (\1)])))

; (str-2-seq "3-598-21507-0")

(defn hdl-check [char]
  (cond
    (= \X char) 10
    (Character/isDigit char) (Character/digit char 10)
    :else 1000000))


; (hdl-check \2)

(defn isbn? [isbn]
  (let [digits (first (str-2-seq isbn))
        checkr (second (str-2-seq isbn))]
    ; (println "digits: " digits " checkr: " checkr)
    (let [nine (first-9 digits)
          chkr (apply hdl-check checkr)]
      ; (println "nine: " nine " chkr: " chkr)
      (let [sum (+ nine chkr)]
        (if (and (= (mod sum 11) 0)
                 (> 100000 sum))
            true
            false)))))


; (isbn? "3-598-21507")
; notes
; if the value is wrapped in a '() use `apply` to operate on it
; else you can operate on it directly.

; help
; 1)
; `map vector` source: https://stackoverflow.com/questions/2588227/is-there-an-equivalent-for-the-zip-function-in-clojure-core-or-contrib
; number conversion source: https://stackoverflow.com/questions/19749624/how-to-cast-a-character-to-int-in-clojure/33157929
; (apply + '(1 2))  ; equivalent to (+ 1 2) https://clojuredocs.org/clojure.core/apply
