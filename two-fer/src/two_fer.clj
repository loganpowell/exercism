(ns two-fer)

(defn two-fer [& args] ;; <- arglist goes here
   (if (or
         (= 0 (count args))
         (= (not (type args))
          java.lang.String))
       (str "One for you, one for me.")
       (str "One for " (first args) ", one for me.")))

; ; PSEUDO
;
; (not (= java.lang.String
;         (type "log")))
; ; test
; (two-fer 0)
; 
; (= "One for you, one for me." (two-fer))
; (= "One for Alice, one for me." (two-fer "Logan Powell"))
; (= "One for Bob, one for me." (two-fer "Bob"))
