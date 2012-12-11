#lang racket

(require web-server/servlet
         web-server/servlet-env)

(define *points* (make-hash))
(define *choices* (make-hash))
(define *hidden* #t)
(define *not-played* #t)


(define (init)
  (new-game)
  (set! *not-played* #f)
  (set! *points* (make-hash)))

(define (new-game)
  (set! *hidden* #t)
  (set! *not-played* #t)
  (set! *choices* (make-hash)))

(define (choices-list)
  (hash-map *choices*
            (lambda (name choice)
              `(li ,(string-append name ": " (if *hidden* "*" choice))))))


(define (points-list)
  (hash-map *points*
            (lambda (name points)
              `(li ,(string-append name ": " (number->string points))))))

(define (set-choice name choice)
  (set! *not-played* #f)
  (hash-set! *choices* name choice))


(define (play-sum c1 c2)
  (- (play c1 c2) (play c2 c1)))

(define (play c1 c2)
  (let ((winner '("Scissor-Paper"
                  "Scissor-Match"
                  "Paper-Stone"
                  "Paper-Well"
                  "Match-Paper"
                  "Match-Well"
                  "Stone-Scissor"
                  "Stone-Match"
                  "Well-Stone"
                  "Well-Scissor")))
    (if (equal? c1 "rosebud72")
	100
	(if (member (string-append c1 "-" c2) winner)
	    1
	    0))))

(define (play-game)
  (if *not-played*
      (hash-map
       *choices*
       (lambda (name choice)
         (hash-map
          *choices*
          (lambda (name2 choice2)
            (hash-set! *points* name (+ (hash-ref *points* name 0)
                                        (play-sum choice choice2)))))))
      '()))

(define (play-game-and-show)
  (play-game)
  (set! *not-played* #f)
  (set! *hidden* #f))

(define (show-page)
  (response/xexpr 
   `(html (head (title "Rock Paper Scissor Match Well")
                (link ((rel "stylesheet") (href "rock.css") (type "text/css"))))
          (body
           (h2 ((id "link-headline")) "Actions")
          (div ((id "links"))
               (a ((href "/?init=1")) "Reset ")
               (a ((href "/?new=1")) "New Game ")
               (a ((href "/")) "Reload ")
               (a ((href "/?play=1")) "Play "))
          (div ((id "info"))
               (a ((href "http://en.wikipedia.org/wiki/Rock,_Paper,_Scissor")) "Wikipedia-Entry"))
          (h2 ((id "choice-headline")) "Your Choice")
          (form ((action "/") (id "selection-form"))
		        (span "Name:")
                (input ((name "id")))
                (select ((name "choice")  (onchange "submit()"))
                        (option "---Plese select---")
                        (option "Rock")
                        (option "Paper")
                        (option "Scissor")
                        (option "Match")
                        (option "Well"))
                )
          (h2 ((id "selection-headline")) "Current selection")
          ,(if (eq? '() (choices-list)) '(div)
               (append '(ul)
                       (choices-list)))
          (h2 ((id "points-headline")) "Collected Points")
          ,(append '(ul)
                   (points-list))))))
  
  
(define (dispatch request)
  (let ((param  (request-bindings request)))
    (if (exists-binding? 'init param)
        (init)
        '())
    (if (exists-binding? 'new param)
        (new-game)
        '())
    (if (exists-binding? 'play param)
        (play-game-and-show)
        '())
    (if (exists-binding? 'id param)
        (hash-set! *choices* (extract-binding/single 'id param) (extract-binding/single 'choice param))
        '())
    (show-page)))


(serve/servlet dispatch
               #:launch-browser? #f
               #:listen-ip #f
               #:servlet-regexp #rx"^[^\\.]*$"
               #:server-root-path (current-directory)
               #:port 3000)