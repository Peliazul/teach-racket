#lang racket

(require web-server/servlet  
         web-server/servlet-env)

(define (check-parameters req)
  (if (exists-binding? 'vote (request-bindings req))
      (upvote
       (extract-binding/single 'vote (request-bindings req)))
	  (add-to-list
       (extract-binding/single 'new (request-bindings req)))))

(define *upvotes* (make-hash))

(define (add-to-list text)
  (hash-set! *upvotes* text 0))

(define (get-upvote text)
  (hash-ref *upvotes* text))

(define (compare-upvotes text1 text2)
  (> (get-upvote text1) (get-upvote text2)))

(define (sorted-list)
  (sort (hash-keys *upvotes*) compare-upvotes))

(define (upvote text)
  (hash-set! *upvotes* text (+ 1 (get-upvote text))))

(define (list-item text)
   `(li ,(string-append text ", Upvotes: " (number->string (get-upvote text)))
        ,(vote-link text)))

(define (list->html l)
  (append '(ol)
          (map list-item l)))	

(define (vote-link text)
   `(a ((href ,(string-append "votes?vote=" text))) "+1"))

(define (reddit req)
  (check-parameters req)
  (response/xexpr 
   `(html (head (title "My Reddit")
               ; (link ((rel "stylesheet") (href "reddit.css")))
               ; uncomment and create reddit.css to add styles to the site
                )
          (body
           (form ((action "votes"))
                 (input ((name "new")))
                 (input ((type "submit") (value "Submit Entry"))))
           ,(list->html (sorted-list))))))

(serve/servlet reddit
               #:listen-ip #f
               #:servlet-path "/reddit"
               #:servlet-regexp #rx"^[^\\.]*$"
               #:server-root-path (current-directory))
