#lang racket
; Hangman
; You have to define the word here
(define word "giraffe")

(define (starred-word word guesses)
  (list->string
   (map (lambda (x) 
          (if (member x guesses)
              x
              #\*))
        (string->list word))))

(define (finished? word guesses)
  (eq? '()
       (filter
        (lambda (x)
          (not (member x guesses)))
        (string->list word))))


(define (play (guesses '()))
  (if (finished? word guesses)
      (list 'Word word 'guesses (length guesses))
      (begin
        (display "Word to guess")
        (newline)
        (display (starred-word word guesses))
        (newline)
        (display "Enter a letter!")
        (play (append guesses (string->list (read-line)))))))
(play)