#lang racket

; replace all the capitalized words,
; esp. server with yout smtp server, and user /password with your credentials there 

(require net/smtp)
(require net/head)

(define start-time (current-seconds))

(define (send-email to subject)
  (smtp-send-message "SERVER"
                     "FROM"
                     (list to)
                     (standard-message-header "FROM"
                                              (list to)
                                              '()
                                              '()
                                              subject)
                     (list "Hey!" 
                           (string-append "Time elapsed:  " 
                                          (number->string (- (current-seconds)
                                                             start-time)) 
                                          " Seconds"))
                     #:auth-user "USER"
                     #:auth-passwd "PASSWORD"))
                     
(define (reminder to)
  (display "Ok, trying to send an email now")
  (send-email adresse "Time is passing")
  (sleep 5)
  (reminder to))
