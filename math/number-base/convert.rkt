#lang racket

; convert numbers from any base to any other

(define (char->value c)
  (int->value (char->integer c)))

; "A" has ASCII value 65, and should be 10
(define (int->value i)
  (- i (if (> i 64) 55 48)))

(define (value->char x)
  (integer->char (+ x
                    (if (> x 9) 55 48))))

(define (list->number l base acc)
  (if (eq? '() l)
      acc
      (list->number (rest l) 
                    base 
                    (+ (first l) (* base acc)))))

(define (from-base base s)
  (list->number 
   (map char->value (string->list s))
   base 
   0))
  
(define (to-base base x (acc '()))
  (if (= x 0)
      (list->string (map value->char acc))
      (to-base base (quotient x base) (cons (remainder x base) acc))))

(define (from-to-base base1 base2 s)
  (to-base base2 (from-base base1 s)))

; usage

(to-base 2 10)
(from-base 2 "100101")
(from-to-base 16 2 "FF")