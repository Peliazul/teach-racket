#lang racket

; Compiler for the Brainf*ck language

(define (compile-bf-command c)
  (cond ((equal? c #\.) "(display (string (integer->char (vector-ref mem mempos))))")
        ((equal? c #\,) "(vector-set! mem mempos (char->integer (first (string->list (read-line)))))")
        ((equal? c #\+) "(vector-set! mem mempos (+ (vector-ref mem mempos) 1))")
        ((equal? c #\-) "(vector-set! mem mempos (- (vector-ref mem mempos) 1))")
        ((equal? c #\>) "(set! mempos (+ mempos 1))")
        ((equal? c #\<) "(set! mempos (- mempos 1))")
        ((equal? c #\[) "(do () ((= 0 (vector-ref mem mempos)) ) ")
        ((equal? c #\]) ")")
        (else "")))
        
(define (compile-bf program)
  (apply string-append
         "(define mem (make-vector 30000))"
         "(define mempos 0)"
         (map compile-bf-command (string->list program))))

; Example

; (compile-bf ",>++++++[<-------->-],[<+>-]<.")