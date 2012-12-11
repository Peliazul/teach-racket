#lang racket

; A position is a list of two numbers, first column then row
(define (make-pos column row)
  (list column row))
(define (column pos)
  (first pos))
(define (row pos)
  (first (rest pos)))
(define 1-9 '(1 2 3 4 5 6 7 8 9))

(define (row-conflicts pos)
  (map (lambda (x) 
         (make-pos x (row pos))) 
       1-9))
(define (column-conflicts pos)
  (map (lambda (x)
         (make-pos (column pos) x))
       1-9))
(define (block-add x)
  (if (< x 4)
      0
      (if (< x 7)
          3
          6)))
(define first-block
  '((1 1) (1 2) (1 3)
    (2 1) (2 2) (2 3)
    (3 1) (3 2) (3 3)))

(define (block-conflicts pos)
  (map (lambda (x)
         (make-pos (+ (column x) (block-add (column pos)))
                   (+  (row x) (block-add (row pos)))))
       first-block))
                      
(define (conflicts pos)
  (append (row-conflicts pos)
          (column-conflicts pos)
          (block-conflicts pos)))

(define (make-board) (make-vector 81))

(define (pos->n pos)
  (+ (- (column pos) 1)
     (* 9 (- (row pos) 1))))

(define (n->pos n)
  (make-pos (+ 1 (modulo n 9))
            (+ 1 (floor (/ n 9)))))

(define (show-board board)
  (show-board-from board 0))
(define (show-board-from board n)
  (if (= n 81)
      (newline)
      (begin
        (if (and (= 0 (modulo n 9)) (< 0 n))
            (newline)
            #f)
        (display (vector-ref board n))
        (show-board-from board (+ n 1)))))

(define (get-board board pos)
  (vector-ref board (pos->n pos)))
          
(define (set-board board pos number)
  (begin 
    (vector-set! board (pos->n pos) number)
    board))



(define (conflict-numbers board pos)
  (filter (lambda (x) (> x 0))
          (map (lambda (x) (get-board board x)) 
               (conflicts pos))))

(define (possible-numbers board pos)
  (filter (lambda(x)
            (not (member
                  x
                  (conflict-numbers board pos))))
          1-9))

(define (first-free board)
  (first-free-from board 0))

(define (first-free-from board n)
  (if (= n 81)
      #f
      (if (= 0 (get-board board (n->pos n)))
          (n->pos n)
          (first-free-from board (+ n 1)))))

(define (find-solution board)
  (if (eq? #f (first-free board))
      board
      (find-solution-from 
       board 
       (first-free board) 
       (possible-numbers board (first-free board)))))

(define (find-solution-from board pos numbers)
  (if (eq? numbers '())
      #f
      (or (find-solution (set-board board pos (first numbers)))
          (find-solution-from (set-board board pos 0) pos (rest numbers)))))

(define board 
  (vector 0 0 4 2 5 0 0 8 0 
          0 0 0 0 9 0 5 0 7 
          0 0 0 3 0 0 0 0 4 
          0 2 0 0 0 6 3 0 0 
          5 3 0 0 0 0 0 7 8 
          0 0 8 5 0 0 0 2 0 
          1 0 0 0 0 2 0 0 0 
          6 0 3 0 1 0 0 0 0 
          0 4 0 0 6 5 9 0 0))

(find-solution board)