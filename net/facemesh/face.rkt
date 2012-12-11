#lang racket

(require web-server/servlet
         web-server/servlet-env)


(define (show-voting-page)
  (let* ( (select (random-votes))
          (links  (votes-to-links select))
          (imgsrc (votes-to-imgsrc select)) )
    `(html (head (title "FaceMesh!!")
                 (link ((rel "stylesheet") (href "face.css") (type "text/css"))))
           (body
            (h2 ,(string-append "Face-Mesh: Who is the coolest? Votings: "
				(number->string *count-votings*)))
            (p "The perfect couples...."
               (a ((href "/couples?top=1")) "top ")
               "and"
               (a ((href "/couples?top=0")) " not so top"))
	       " or simply "
	       (a ((href "/")) " continue"))
            (table
             (tr
              (td ((valign "top"))
                  (a ((href ,(first links)))
                     (img ((src ,(first imgsrc)))))
		  (br)
		  ,(number->string (hash-ref *votings* (first select))))
              (td ((valign "top"))
                  (a ((href ,(second links)))
                     (img ((src ,(second imgsrc)))))
		  (br)
		  ,(number->string (hash-ref *votings* (first select)))))))))


(define (show-couples-page couples)
  `(html
    (head (title "FaceMesh!!")
          (link ((rel "stylesheet") (href "face.css") (type "text/css"))))
    (body
     (h2 "The perfect couples")
     ,(append `(p)
              (map (lambda (x)
                     `(div ((class "couple"))
                       (span ((class "couple-left"))  (img ((src ,(imgsrc (first x))))))
                       (span ((class "couple-right")) (img ((src ,(imgsrc (second x))))))))
                   couples)))))


(define *votings* (make-hash))
(define *count-votings* 0)

(define (rating name)
  (hash-ref *votings* name 250))

(define (voting s1 s2 first-wins) ; first-wins is 1 if s1 wins, otherwise 0
  (set! *count-votings* (+ 1 *count-votings*))
  (hash-set! *votings* s1 (ra-new (rating s1) (rating s2) first-wins))
  (hash-set! *votings* s2 (ra-new (rating s2) (rating s1) (- 1 first-wins))))


(define (show-ratings)
  (hash-map *votings*
            (lambda (name rating)
               (display (format "~a:~a~n" (vector-ref *students* name) rating)))))


; formula from the movie "The social network"
(define (ea ra rb)
  (/ 1
     (+ 1
        (expt 10
              (/ (- rb ra)
                 400)))))

(define (ra-new ra rb first-wins) 
  (+ ra (* 50 ; our  "K", smaller for more stable changes
           (- first-wins (ea ra rb)))))

(define (vote click)
    (apply voting (map string->number (regexp-split #rx"-" click))))

(define (random-votes)
  (let ( (p1 (random (vector-length *students*)))
         (p2 (random (vector-length *students*))))
    (if (or (= p1 p2)
            (not (equal? (substring (vector-ref *students* p1) 0 1)
                         (substring (vector-ref *students* p2) 0 1))))
        (random-votes)
        (list p1 p2))))

(define (votes-to-links l)
  (list (format "/?vote=~a-~a-1" (first l) (second l))
        (format "/?vote=~a-~a-0" (first l) (second l))))

(define (imgsrc x)
  (format "/pics/~a" (vector-ref *students* x)))
(define (votes-to-imgsrc l)
  (map imgsrc l))


(define (top-list gender topfirst)
  (filter (lambda (x)
            (equal? gender (substring (vector-ref *students* x) 0 1)))
          (sort  (hash-map *votings* (lambda (x y) x))
                 (lambda (x y) ((if topfirst > <) (rating x) (rating y))))))
(define (first-couples girls boys (n 10))
  (if (or (= n 0) (eq? girls '()) (eq? boys '()))
      '()
      (cons (list (first girls) (first boys))
            (first-couples (rest girls) (rest boys) (- n 1)))))

(define (top-couples topfirst)
  (let ( (girls (top-list "g" topfirst))
         (boys  (top-list "b" topfirst)) )
    (first-couples girls boys)))



(define *students*
  (apply vector
   (map (lambda (x) (path->string x)) (directory-list (string->path "htdocs/pics")))))

(define (dispatch request)
  (let ((param  (request-bindings request)))
    (if (exists-binding? 'vote param)
        (vote (extract-binding/single 'vote param))
        '())
    (if (exists-binding? 'top param)
        (show-couples-page (top-couples (equal? "1" (extract-binding/single 'top param))))
        (show-voting-page))))

(define (start)
  (serve/servlet dispatch
                 #:launch-browser? #f
                 #:listen-ip #f
                 #:servlet-regexp #rx"^[^\\.]*$"
                 #:server-root-path (current-directory)
                 #:port 3001))