#lang racket

(require scheme/tcp)

(define-struct pos (x y)                         #:transparent)
(define-struct element (type id pos)             #:mutable)
(define-struct client (id from-client to-client) #:mutable)

(define *game-name* "")
(define *max-pos* (make-pos 0 0))
(define *start-time* -1)
(define *boxes-left* 0)
(define *tcp-listener* '())
(define *clients*  '())
(define *elements* '())
(define *filename* "")

(define (find fun l)
  (if (eq? l '())
      #f
      (if (fun (first l))
          (first l)
          (find fun (rest l)))))

(define (elements-at-pos p1)
  (filter (lambda (e)
	    (equal? p1 (element-pos e)))
	  *elements*))
(define (element-is? e type)
  (equal? type (element-type e)))
(define (element-is-box? e)
  (element-is? e "B"))
(define (element-is-target? e)
  (element-is? e "T"))
(define (element-is-player? e)
  (element-is? e "P"))

(define (pos-empty-or-target? p1)
  (not 
   (find (lambda (e)
           (not (element-is-target? e)))
         (elements-at-pos p1))))

(define (pos-empty? p1)
  (eq? '() (elements-at-pos p1)))

(define (pos-box? p1)
  (find element-is-box? (elements-at-pos p1)))

(define (game-players)
  (filter element-is-player? *elements*))

(define (normalize n lower upper)
  (min upper (max lower n)))
(define (make-valid-pos p)
  (make-pos (normalize (pos-x p) 0 (pos-x *max-pos*))
	    (normalize (pos-y p) 0 (pos-y *max-pos*))))
(define (move-direction p dir)
  (make-valid-pos
   (cond ((equal? dir "UP")    (make-pos (pos-x p) (+ (pos-y p) 1)))
	 ((equal? dir "DOWN")  (make-pos (pos-x p) (- (pos-y p) 1)))
	 ((equal? dir "LEFT")  (make-pos (- (pos-x p) 1) (pos-y p)))
	 ((equal? dir "RIGHT") (make-pos (+ (pos-x p) 1) (pos-y p))))))

(define (possible-move? p dir)
  (let ((new-pos (move-direction p dir)))
    (or
     (pos-empty-or-target? new-pos)
     (and (pos-box? new-pos)
	  (pos-empty-or-target? (move-direction new-pos dir))))))

; the "PUT"-Message for position p
(define (game-put-message e)
   (format "PUT ~a ~a ~a ~a"
           (element-type e)
           (element-id e)
           (pos-x (element-pos e))
           (pos-y (element-pos e))))
(define (game-put-messages-pos p)
  (let ( (e (elements-at-pos p)))
    (if (eq? '() e)
        (list (game-put-message (make-element "E" 0 p)))
        (map game-put-message e))))

(define (game-put-messages-all)
  (map game-put-message *elements*))

; Moving a box from->to
(define (game-update-boxes-left from to)
  (let ((from-is-target (find-element-pos "T" from))
		(to-is-target (find-element-pos "T" to)))
    (if to-is-target
		(if (not from-is-target)
            (set! *boxes-left* (- *boxes-left* 1))
            '())
        (if from-is-target
            (set! *boxes-left* (+ *boxes-left* 1))
            '()))))

(define (game-move-client client dir)
  (let* ((player (find-element "P" (client-id client)))
		 (player-pos (element-pos player))
		 (new-pos (move-direction player-pos dir))
		 (new-new-pos (move-direction new-pos dir))
		 (move-box (find-element-pos "B" new-pos)))
    (if (possible-move? player-pos dir)
        (begin
          (set-element-pos! player new-pos)
          (if move-box
              (begin
                (set-element-pos! move-box new-new-pos)
                (game-update-boxes-left new-pos new-new-pos)
                (broadcast-messages (game-put-messages-pos new-new-pos)))
              '())
          (broadcast-messages
           (append (game-put-messages-pos player-pos)
                   (game-put-messages-pos new-pos))))
	'())))

(define (element-match e type id)
  (and (equal? type (element-type e))
       (equal? id   (element-id e))))

(define (find-element type id)
  (find (lambda (e)
		  (element-match e type id))
		*elements*))

(define (find-element-pos type pos)
  (find (lambda (e)
		  (equal? type (element-type e)))
		(elements-at-pos pos)))


(define (game-missing-players)
  (let ( (player-ids (map element-id (game-players)))
		 (client-ids (map client-id    *clients*)))
    (filter (lambda (id) (not (member id client-ids)))
			player-ids)))

(define (game-running?)
  (< 0 *start-time*))

(define (game-waiting-message)
  (string-join
   (append (list (format "GAME ~a ~a ~a WAITFOR " *game-name* (pos-x *max-pos*) (pos-y *max-pos*)))
		   (game-missing-players))
   " "))

(define (client-message client message)
  (display (format "Send to client ~a message ~a~n" (client-id client) message))
  (write-string (string-append message "\n") (client-to-client client))
  (flush-output (client-to-client client)))


(define (client-messages client messages)
  (for ( (m messages) )
	   (client-message client m)))

(define (broadcast-message message)
  (for ( (client *clients*) )
	   (client-message client message)))


(define (broadcast-messages messages)
  (for ( (m messages) )
	   (broadcast-message m)))


(define (game-join-client client)
  (if (eq? '() (game-missing-players))
      (client-message client "REJECT")
      (begin
		(client-message client "ACCEPT")
		(set-client-id! client (first (game-missing-players)))
        (if (eq? '() (game-missing-players))
            (begin
              (set! *start-time* (current-seconds))
              (broadcast-messages (append (list (format "START ~a ~a ~a" *game-name* (pos-x *max-pos*) (pos-y *max-pos*)))
                                          (game-put-messages-all))))
            '()))))

(define (game-over?)
  (= 0 *boxes-left*))


(define (game-elapsed)
  (- (current-seconds) *start-time*))

(define (game-status-message)
  (format "BOXES ~a ELAPSED ~a" *boxes-left* (game-elapsed)))

(define (poll-clients)
  (if (tcp-accept-ready? *tcp-listener*)
      (let-values ( ( (from-client to-client)
		      (tcp-accept *tcp-listener*)) )
		(set! *clients*
			  (cons (make-client 0 from-client to-client)
					*clients*)))
      '())
  (for ( (c *clients*) )
       (if (char-ready? (client-from-client c))
		   (let ( (line (read-line (client-from-client c))) )
             (if (not (eof-object? line))
                 (let* (  (in (first (regexp-match #rx"[A-Za-z 0-9]+" line)))
						  (l  (regexp-split #rx"[ \n]+" in)))
                   (process-client-message c (first l) (rest l)))
                 '()))
           '()))
  (if (game-over?)
      (broadcast-message (format "GAMEOVER ~a" (game-elapsed)))
      (begin
		(sleep 0.01)
		(poll-clients))))

; create a new id
(define (make-id-fun)
  (let ( (n (make-hash)))
    (lambda (x)
      (hash-set! n x (+ 1 (hash-ref n x 0)))
      (number->string (hash-ref n x)))))
(define new-id (make-id-fun))

(define (line->elements l lineno (colno 1))
    (if (eq? l '())
      '()
      (let ( (e (first l))) 
        (append
         (map (lambda (x) (make-element x (new-id x) (make-pos colno lineno)))
              (cond ((eq? e #\#) (list "W"))
                    ((eq? e #\.) (list "T"))
                    ((eq? e #\@) (list "P"))
                    ((eq? e #\+) (list "P" "T"))
                    ((eq? e #\$) (list "B"))
                    ((eq? e #\*) (list "B" "T"))
                    (else '())))
         (line->elements (rest l) lineno (+ 1 colno))))))

(define (make-positions l (lineno 1))
  (if (eq? l '())
      '()
      (append
       (line->elements (first l) lineno)
       (make-positions (rest l) (+ 1 lineno)))))


(define (file->list port)
  (let ( (l (read-line port)) )
    (if (eq? l eof)
        '()
        (cons (string->list l)
              (file->list port)))))
(define (strip-comments l)
  (filter (lambda (x) (not (equal? "#" (first x)))) l))

(define (init-from-file)
  (let* ( (lines (file->list (open-input-file *filename*)))) 
    (set! *elements*     (make-positions lines))
    (set! *boxes-left*   (length (filter element-is-box? *elements*)))
    (set! *game-name*    *filename*)
    (set! *max-pos*      (make-pos 
                          (apply max (map (lambda (e) (pos-x (element-pos e))) *elements*))
                          (apply max (map (lambda (e) (pos-y (element-pos e))) *elements*))))))

    

(define (process-client-message client command params)
  (display (format "Got command ~a from client~n" command))
  (cond ((equal? "STATUS" command)
           (if (game-running?)
               (client-message client (game-status-message))
               (client-message client (game-waiting-message))))
	((equal? "JOIN" command)
	 (game-join-client client))
        ((equal? "RESET" command)
         (begin
           (init-from-file)
           (broadcast-messages (game-put-messages-all))))
        ((equal? "REDRAW" command)
         (client-messages client (game-put-messages-all)))
	((member command '("LEFT" "RIGHT" "UP" "DOWN"))
	 (game-move-client client command))))

(define (start-game-server port game-file)
  (set! *tcp-listener* (tcp-listen port))
  (set! *filename* game-file)
  (set! *clients*      '())
  (init-from-file)
  (poll-clients))
