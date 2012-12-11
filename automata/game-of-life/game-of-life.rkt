#lang racket

(define (2d->1d x y)
  (+ x (* y WIDTH)))
(define (vector-ref-2d vec x y)
  (vector-ref vec (2d->1d x y)))
(define (vector-set!-2d vec x y value)
  (vector-set! vec (2d->1d x y) value))
(define (vector-ref-2d-safe vec x y)
 (if (or (< x 0) (< y 0) (>= x WIDTH) (>= y WIDTH))
     0
 (vector-ref-2d vec x y)))


(define WIDTH 100)
(define PIXELWIDTH 3)
(require graphics)
(open-graphics)


; Screen
(define (init-window)
  (open-viewport "The game of life" (* WIDTH PIXELWIDTH) (* WIDTH PIXELWIDTH)))


(define (sum-neighbours vec x y)
  (+
   (vector-ref-2d-safe vec (- x 1)(- y 1)) 
   (vector-ref-2d-safe vec x (- y 1)) 
   (vector-ref-2d-safe vec (+ x 1)(- y 1)) 
   (vector-ref-2d-safe vec (- x 1) y) 
   (vector-ref-2d-safe vec (+ x 1) y) 
   (vector-ref-2d-safe vec (- x 1)(+ y 1)) 
   (vector-ref-2d-safe vec x (+ y 1)) 
   (vector-ref-2d-safe vec (+ x 1)(+ y 1))
   ))


(define (next-generation x sum)
  (if (or
       (and (= x 0 )
            (= sum 3))
       (and (= x 1)
            (or (= sum 2)
                (= sum 3))))
      1
      0))


(define (next-generation-vector vec1 vec2)
  (for* ((x (in-range 0 WIDTH))
         (y (in-range 0 WIDTH)))
    (vector-set!-2d vec2 x y
                    (next-generation (vector-ref-2d vec1 x y) (sum-neighbours vec1 x y)))))


(define (show-vector v w)
  (for* ((x (in-range 0 WIDTH))
         (y (in-range 0 WIDTH)))
    (((if (= 1 (vector-ref-2d v x y))
          draw-solid-rectangle
          clear-solid-rectangle) w)
     (make-posn (* PIXELWIDTH x)
                (* PIXELWIDTH y))
     PIXELWIDTH
     PIXELWIDTH)))


(define (make-game-vector)
  (make-vector (* WIDTH WIDTH)))


(define (game-of-life w vec1 vec2)
  (show-vector vec1 w)
  (if (not (ready-mouse-click w))
      (begin
        ; (sleep 0.1)
        (next-generation-vector vec1 vec2)
        (game-of-life w vec2 vec1))
      '()))


(define vec1 (make-game-vector))
(vector-set!-2d vec1 50 50 1)
(vector-set!-2d vec1 51 50 1)
(vector-set!-2d vec1 49 51 1)
(vector-set!-2d vec1 50 51 1)
(vector-set!-2d vec1 50 52 1)


(game-of-life (init-window)
              vec1
              (make-game-vector))
