#lang racket

; an interpreter for the Brainf*ck language

(define count 0) ; steps performed

(define (bf mem code mem-pos code-pos)
  (set! count (+ 1 count))
  (if (= code-pos (vector-length code))
      (format "stop after  ~a steps" count)
      (let ( (command (vector-ref code code-pos)) )
        (cond ( (equal? command #\>)
                (bf mem code (+ mem-pos 1) (+ code-pos 1))) 
              ( (equal? command #\<)
                (bf mem code (- mem-pos 1) (+ code-pos 1)))
              ( (equal? command #\+)
                (begin
                  (vector-set! mem mem-pos (+ 1 (vector-ref mem mem-pos)))
                  (bf mem code mem-pos (+ 1 code-pos))))
              ( (equal? command #\-)
                (begin
                  (vector-set! mem mem-pos (- (vector-ref mem mem-pos) 1))
                  (bf mem code mem-pos (+ 1 code-pos))))
              ( (equal? command #\.)
                (begin
                  (display (integer->char (vector-ref mem mem-pos)))
                  (bf mem code mem-pos (+ code-pos 1))))
              ( (equal? command #\,)
                (begin
                  (vector-set! mem mem-pos (char->integer (first (string->list (read-line)))))
                  (bf mem code mem-pos (+ code-pos 1))))
              ( (equal? command #\[)
                (bf mem code mem-pos
                (if (= 0 (vector-ref mem mem-pos))
                    (find-loop-end code (+ code-pos 1) 0)
                    (+ code-pos 1))))
              ( (equal? command #\])
                (bf mem code mem-pos
                    (if (< 0 (vector-ref mem mem-pos))
                        (find-loop-begin code (- code-pos 1) 0)
                        (+ code-pos 1))))
              ( else (bf mem code mem-pos (+ 1 code-pos)))))))
       
; find the closing bracket "]", beginning at pos. 
(define (find-loop-end code pos nesting)
  (if (equal? (vector-ref code pos) #\])
      (if (= nesting 0)
          (+ pos 1)
          (find-loop-end code (+ 1 pos) (- nesting 1)))
      (if (equal? (vector-ref code pos) #\[)
          (find-loop-end code (+ 1 pos) (+ nesting 1))
          (find-loop-end code (+ 1 pos) nesting))))

; find the opening  bracket "[", beginning at pos. 
(define (find-loop-begin code pos nesting)
  (if (equal? (vector-ref code pos) #\[)
      (if (= nesting 0)
          (+ pos 1)
          (find-loop-begin code (- pos 1) (- nesting 1)))
      (if (equal? (vector-ref code pos) #\])
          (find-loop-begin code (- pos 1) (+ nesting 1))
          (find-loop-begin code (- pos 1) nesting))))


(define (call-bf program)
  (set! count 0)
  (bf (make-vector 30000)
      (apply vector (string->list program))
      0
      0))

;


; Read two digits and print the (single digit) sum
;(call-bf ",>++++++[<-------->-],[<+>-]<.")

; print a text
;(call-bf "+++[>+++++<-]>[>+>+++>+>++>+++++>++<[++<]>---]>->-.[>++>+<<--]>--.--.+.>>>++.<<.<------.+.+++++.>>-.<++++.<--.>>>.<<---.<.-->-.>+.[+++++.---<]>>[.--->]<<.<+.++.++>+++[.<][.]<++.")

; Print squares up to 10000
; (call-bf "++++[>+++++<-]>[<+++++>-]+<+[>[>+>+<<-]++>>[<<+>>-]>>>[-]++>[-]+>>>+[[-]++++++>>>]<<<[[<++++++++<++>>-]+<.<[>----<-]<]<<[>>>>>[>>>[-]+++++++++<[>-<-]+++++++++>[-[<->-]+[<<<]]<[>+<-]>]<<-]<<-]")


