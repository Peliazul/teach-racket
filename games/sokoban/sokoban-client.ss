#lang racket

(require graphics)
(open-graphics)

(define *window* #f)
(define *tile-size* 20)

(define *to-server* #f)
(define *from-server* #f)
(define *game-running* #f)

(define *colors* (make-hash))
(hash-set! *colors* "E" (make-rgb 1 1 1))
(hash-set! *colors* "B" (make-rgb 0.6 0.7 0.4))
(hash-set! *colors* "P" (make-rgb 0.8 0.1 0.2))
(hash-set! *colors* "T" (make-rgb 0.1 0.1 0.8))
(hash-set! *colors* "W" (make-rgb 0   0   0  ))

(define (send-message msg)
  (write-string msg *to-server*)
  (flush-output *to-server*))

(define (connect server port)
   (let-values ( ( (from to)
                   (tcp-connect server port)) )
     (set! *from-server* from)
     (set! *to-server* to)
     (send-message (format "JOIN~n"))
     (wait-for-start)))

(define (wait-for-start)
  (poll-server)
  (if *game-running*
      (event-loop)
      (wait-for-start)))

(define (start-game name maxx maxy)
  (set! *window* (open-viewport (string-append "Multi-Sokoban: " name)
                                (* maxx *tile-size*)
                                (* maxy *tile-size*)))
  (set! *game-running* #t))


(define (put-element type x y)
  (let ((pos   (make-posn (* (- x 1) *tile-size*) (* (- y 1) *tile-size*)))
        (color (hash-ref *colors* type #f)))
    (if color
        (begin
          ((draw-solid-rectangle  *window*) pos *tile-size* *tile-size* color)
          (display (format "Draw type ~a at ~a x ~a ~n" type x y)))
        (display (format "Unknown type ~a~n" type)))))
  

(define *key-messages* (make-hash))
(hash-set! *key-messages* 'up    "DOWN")
(hash-set! *key-messages* 'down  "UP")
(hash-set! *key-messages* 'left  "LEFT")
(hash-set! *key-messages* 'right "RIGHT")

(define (process-key-event key)
  (let ((msg (hash-ref *key-messages* key #f)))
    (if msg
        (send-message (format "~a~n" msg))
        (display (format "Unknown Key: ~a~n" key)))))

(define (poll-keyboard)
  (let ( (key (ready-key-press *window*)) )
    (if key 
        (process-key-event (key-value key))
        '())))

(define (poll-server)
  (if (char-ready? *from-server*)
      (process-server-message (read-line *from-server*))
      '()))


(define (event-loop)
  (if *game-running*
      (begin
        (poll-server)
        (poll-keyboard)
        (sleep 0.01)
        (event-loop))
      (display "Goodbye")))

(define (process-server-message message)
  (let* ( (parts (regexp-split #rx" +" message)) 
          (command (first parts))
          (params (apply vector (rest parts))))
    (cond ((equal? command "PUT") (put-element (vector-ref params 0) 
                                               (string->number (vector-ref params 2))
                                               (string->number (vector-ref params 3))))
          ((equal? command "GAMEOVER") (begin 
                                        (display message)
                                        (set! *game-running* #f)))
          ((equal? command "START") (start-game (vector-ref params 0) 
                                               (string->number (vector-ref params 1))
                                               (string->number (vector-ref params 2)))))))
           

  
  
