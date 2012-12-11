#lang racket

(define width 200) ; how many cells
(define size 2) ; size of the square

(define old (make-vector size)) ; the previous states
(define new (make-vector size)) ; the current states


(define rules (vector 0 1 0 1 0 0 1 1))

(require graphics)
(open-graphics)
(define w (open-viewport "The secret of life" (* width size) (* width size)))
((clear-viewport w))

(define (draw-dead x y)
  ((draw-solid-rectangle w) 
   (make-posn (* x size) (* y size))  size size (make-rgb 0 0 0)))

(define (new-cell-state l m r)
  (vector-ref rules
              (+ r (* 2 m) (* 4 l))))


(define (state-of-cell x) 
  (if (or (< x 0) (>= x size))
      0 ; assume dead for cells outside of range
      (vector-ref alt x)))

(define (set-random)
  (for ( (i (in-range 0 size)))
    (vector-set! old i (random 2))))

(define (generation)
  (for ((i (in-range 0 width)))
    (vector-set! new i (new-cell-state (state-of-cell (- i 1))
                             (state-of-cell  i)
                             (state-of-cell (+ i 1)))))
  (vector-copy! old 0 new))


(define (draw-generation y)
  (for (( i (in-range 0 width)))
    (if (= 0 (state-of-cell i))
        (draw-dead i y)
        '())))

(define (draw-generations)
  (for ((y (in-range 0 width)))
    (draw-generation y)	
    (generation)))
                                         

(set-random)
(draw-generations)
