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

; first attempt
;
; (defn str-2-seq [str]
;   (let [chars (filter #(not= \- %) (seq str))]
;     (if (= 10 (count chars))
;         (let [digits (first (split-at 9 chars))
;               checkr (second (split-at 9 chars))]
;           ; (println "digits: " digits " checkr: " checkr))
;           [digits checkr])
;         "No bueno")))


; further refactoring
; You could use `(assert ...)` in `str-2-seq` instead of the `if` to perhaps simplify things there (now that you're catching assertions):

(defn str-2-seq [str]
  (let [chars (filter #(not= \- %) (seq str))]
    (assert (= 10 (count chars)))
    (split-at 9 chars)))

; (str-2-seq "3-598-21507-0")

; original:
; (defn first-9 [seq]
;   (if
;     (every? #(Character/isDigit %) seq)
;     (let [vects (map vector (reverse (range 2 11)) (map #(Character/digit % 10) seq))]
;       (let [products (map #(apply * %) vects)]
;         (apply + products)))
;     1))

; refactor based on clojurians exchange:
; HERE => https://clojurians.slack.com/archives/C053AK3F9/p1523483259000369
; use `range`s 'step' function
; use `map`s ability to sort of "zip together" multiple sequences and apply a function to the zippered sets (will terminate with the shortest sequence)
; to throw an AssertionError, use `{:pre [...]}

(defn first-9 [digits]
  {:pre [(= 9 (count digits)) (every? #(Character/isDigit %) digits)]}
  (reduce + (map * (range 10 1 -1) (map #(Character/digit % 10) digits))))

; (time (first-9 '(\1 \2 \3 \4 \5 \6 \7 \8 \9)))


(defn hdl-check [char]
  (cond
    (= \X char) 10
    (Character/isDigit char) (Character/digit char 10)
    :else "Bad baby"))


; (hdl-check \2)

; original:
; (defn isbn? [isbn]
;   (let [digits (first (str-2-seq isbn))
;         checkr (second (str-2-seq isbn))]
;     ; (println "digits: " digits " checkr: " checkr)
;     (let [nine (first-9 digits)
;           chkr (apply hdl-check checkr)]
;       ; (println "nine: " nine " chkr: " chkr)
;       (let [sum (+ nine chkr)]
;         (if (and (= (mod sum 11) 0)
;                  (> 100000 sum))
;             true
;             false)))))

; refactor based on clojurians exchange:
; use a `try catch` at the top level, not at the sub assemblies to bubble up a true false
; use destructuring to avoid calling `str-2-seq` twice:
; You have a cascade of three `let`s that could all just be one.)
; Also `(if condition true false)` is the same as just `condition`.

; (defn isbn? [isbn]
;   (try
;     (let [[digits checkr] (str-2-seq isbn)
;           nine (first-9 digits)
;           chkr (apply hdl-check checkr)
;           sum (+ nine chkr)]
;       (and (= (mod sum 11) 0)
;            (> 100000 sum)))
;     (catch Throwable _ false)))

; refactoring even further:
; Then another little trick you can do in `isbn?` is to further destructure `checkr` since you know it should be a sequence of one element

(defn isbn? [isbn]
  (try
    (let [[digits [checkr]] (str-2-seq isbn)
          nine (first-9 digits)
          chkr (hdl-check checkr) ; checkr is a Character now -- no need for apply
          sum (+ nine chkr)]
      (and (= (mod sum 11) 0)
           (> 100000 sum)))
    (catch Throwable _ false)))


; also see `some->>`: https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/some-%3E%3E
; `(boolean (some->> input (check-1) (check-2) ... (check-n)))`
;     where each check function returns either a map of bindings
;     (including ones created by previous steps), or nil
;     (to indicate some condition wasn't met and it failed)
;     `boolean` turns `nil` into `false`


; (isbn? "3-598-21507")


; notes
; if the value is wrapped in a '() use `apply` to operate on it
; else you can operate on it directly.

; help
; 1)
; `map vector` source: https://stackoverflow.com/questions/2588227/is-there-an-equivalent-for-the-zip-function-in-clojure-core-or-contrib
; number conversion source: https://stackoverflow.com/questions/19749624/how-to-cast-a-character-to-int-in-clojure/33157929
; (apply + '(1 2))  ; equivalent to (+ 1 2) https://clojuredocs.org/clojure.core/apply
