#lang racket

(define answers '("Ok, go on!"
                  "Could this have something to do with your mother?"
                  "Stick to the subject!"
                  "This sounds interesting."))

(define (pick-random l)
  (if (< (random) (/ 1 (length l)))
      (first l)
      (pick-random (rest l))))

(define (eliza)
  (display (pick-random answers))
  (set! answers (append answers (list (read-line))))
  (newline)
  (eliza))

