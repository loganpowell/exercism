(ns bob)

; let's try decomposing this a bit...

; YAY!!! Nope.

; (defn reduce [f result coll]
;   (if (not= '() coll)
;     (reduce f (f result (first coll)) (rest coll))
;     result))
;
; (defn filtering [pred]
;   (fn [rf]
;     (fn [result el]
;       (if (pred el)
;         (rf result el)
;         result))))

; (every? #(Character/isUpperCase %)
;   (reduce ((filter #(Character/isLetter %)) conj) '()
;     '(\H \E \L \L \0 \!)))
;
; (reduce ((filtering #(Character/isLetter %)) conj) '() '(\H \e \l \0 \!))
; rubber : road

(defn response-for [& string] ;; <- arglist goes here
  (let [chars (seq (first string))
        lack? (or
                (empty? chars)
                (every? #(= \space %) chars))
        ques? (and
                (= \? (last chars))
                (not
                  (every? #(Character/isUpperCase %)
                    (reduce ((filter #(Character/isLetterOrDigit %)) conj) '()
                      chars))))
        yell? (and
                (some #(Character/isLetter %) chars)
                (every? #(Character/isUpperCase %)
                  (reduce ((filter #(Character/isLetter %)) conj) '()
                    chars)))]
    (cond
      lack? "Fine. Be that way!"
      ques? "Sure."
      yell? "Whoa, chill out!"
      :else "Whatever.")))

  ;; your code goes here
; ; tests
; (response-for "AAA?")
;
; (seq "hel   LLLlo")
;
; (for '(h e l l o) println)
;
; (for [char '("h" "?") :when (= "?" char)] char)
;
; (some #(= \? %) '(\h \i \?))
; (every? #(Character/isUpperCase %) '(\H \e))
;
; (Character/isLetter (first '(\a \b)))
;
; ; substituting...
;
; (cons (if (Character/isLetter (first '(\a \b)))
;         (first '(\a \b))
;         nil)
;       '())
