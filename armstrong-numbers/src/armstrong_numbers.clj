(ns armstrong-numbers)

(defn exp [x n]
  (reduce * (repeat n x)))

(defn armstrong? [n]
  (let [parts (seq (str n))
        count (count parts)
        each (map #(- (int %) 48) parts)]
    (if (= n
           (reduce + (map #(exp % count) each)))
        true
        false)))

; PSEUDO CODING DRAFTS

; ; sum of it's own digits (n = count number of digits)

; (time (count (partition 1 (str 12)))) ; nope. doesn't work

; (time (map #(- (int %) 48)(seq (str 12)))) ; works!

; ; x = each digit is broken out and raised to n
; ; the sum of each x = compared number

; (reduce + (map #(exp % 2) '(1 2))) ; works!
;
; ; condition: if number = compared number => armstrong = true, else false
; (armstrong? 154) ; booyah
