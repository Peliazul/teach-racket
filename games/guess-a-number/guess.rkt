#lang racket

(require web-server/servlet  
         web-server/servlet-env)

(define (game req)
  (if (exists-binding? 'play (request-bindings req))
      (extract-binding/single 'play (request-bindings req))
      "New"))

(define (update req)
  (if (equal? (game req) "smaller")
      (smaller)
      (if (equal? (game req) "larger")
          (larger)
          (start))))

(define (info-webseite req)
  (update req)
  (response/xexpr 
   `(html (head (title "Guess"))
          (body (p (span "Computer says: ") 
                   (span ,(number->string (guess))))
                (p (span "Number of guesses: ")
                   (span ,(number->string number-of-guesses)))
                (p (a ((href "?play=smaller")) "smaller ")
                   (a ((href "?play=larger")) "larger ")
                   (a ((href "?play=correct")) "correct ")
                   (a ((href "?play=new")) "new"))))))

(define smallest 0)
(define largest 0)
(define number-of-guesses 0)

(define (start)
  (set! smallest 1)
  (set! largest 1000)
  (set! number-of-guesses 0))

(define (guess)
  (begin
	(set! number-of-guesses (+ 1 number-of-guesses)))
	(quotient (+ smallest largest) 2))


(define (smaller)
  (set! largest (guess)))

(define (larger)
  (set! smallest (guess)))

(start)

(serve/servlet info-webseite #:port 3000)