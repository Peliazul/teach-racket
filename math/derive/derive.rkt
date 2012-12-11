#lang racket

(define rules 
  (make-hash
   (list
    (cons '+  (lambda (x params)
                (append '(+)
                        (map (lambda (summand)
                               (derive summand x))
                             params))))
    (cons '-  (lambda (x params)
                (append '(-)
                        (map (lambda (summand)
                               (derive summand x))
                             params))))
    (cons 'sin   (lambda (x params)
                   `(* (cos ,(first params)) ,(derive (first params) x))))
    (cons 'cos   (lambda (x params)
                   `(* -1 (sin ,(first params)) ,(derive (first params) x))))
    (cons 'log   (lambda (x params)
                   `(/ ,(derive (first params) x)
                       ,(first params))))
    (cons '/     (lambda (x params)
                   (let ((g (first params))
                         (h (second params)))
                     `(/ (- (* ,(derive g x) ,h)
                            (* ,(derive h x) ,g))
                         (expt ,h 2)))))
    (cons '*     (lambda (x params)
                   `(+ (* ,(first params) ,(derive (second params) x))
                       (* ,(derive (first params) x) ,(second params)))))
    (cons 'expt  (lambda (x params)
                   (let ((g (first params))
                         (h (second params)))
                     `(* 
                       (+ 
                        (* ,(derive h x)
                           (log ,g))
                        (* ,h
                           (/ ,(derive g x)
                           ,g)))
                       (expt ,g ,h))))))))

(define derive 
  (lambda (formula x)
    (if (list? formula)
        ((hash-ref rules (first formula)) x (rest formula))
        (if (equal? formula x)
            1
            0))))

(derive '(sin (* x x)) 'x)