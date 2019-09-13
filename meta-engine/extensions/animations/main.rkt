#lang racket

(provide timeline)

(define-syntax-rule 
  (timeline #:time time 
            #:normal normal-state 
            (change-to state #:at at #:for for) ...)
   
  (cond
    [(and (> time at)
          (< time (+ at for)))
     state]
    ...
    [else normal-state]))

