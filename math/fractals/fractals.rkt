#lang racket

(define max-x 1000)
(define max-y 1000)
(define max-iter 255)

(define (^2 x) (* x x))

(define (iterate-step c zn zn+1 iteration)
  (if (or (> iteration max-iter) (> (magnitude zn) 4))
      iteration
      (iterate-step c (zn+1 c zn) zn+1 (+ 1 iteration))))

(define (iterate z0 zn+1)
  (iterate-step z0 0 zn+1 0))

(define (mandelbrot x y)
  (iterate (make-rectangular x y)
           (lambda (c zn) (+ c (^2 zn)))))

(define (burning-ship x y)
  (iterate (make-rectangular x y)
           (lambda (c zn)
             (+ c
                (^2 (make-rectangular
                     (abs (real-part zn))
                     (- (abs (imag-part zn)))))))))

(define (write-colors xy->color startx endx stepx starty endy stepy)
  (for* ((y (in-range starty endy stepy))
         (x (in-range startx endx stepx)))
    (printf "~a " (xy->color x y))))


(define (write-pnm-file filename xy->color x1 x2 y1 y2)
  (with-output-to-file (string->path filename)
    (lambda ()
      (begin
        (printf "P2 ")
        (printf "~a ~a " (+ 1 max-x) (+ 1 max-y))
        (printf "~a " max-iter)
        (write-colors xy->color
                      x1 x2
                      (/ (- x2 x1) max-x)
                      y1 y2
                      (/ (- y2 y1) max-y))))))

; (write-pnm-file "/home/cl/temp/mandelbrot.pnm" mandelbrot -2 2 -2.0 2.0)
; (write-pnm-file "/home/cl/temp/burning-ship.pnm" burning-ship -1.8 -1.7 -0.015 0.06)