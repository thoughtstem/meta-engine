#lang racket

(provide timeline
         stream-flow
         grow-pop
         const-stream)

(require ease)

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


(define (grow-pop n m)
  (define grow (ease-real 
                 (polynomial-ease 2)
                 0 6/5 
                 n))

  (define shrink (ease-real 
                   linear-ease
                   #;
                   (polynomial-ease 2) 
                   6/5 1
                   m))

  (stream-append
    grow
    shrink))

(define (const-stream val n)
  (stream-map
    (const val)
    (stream-take (in-naturals) n)))

(define (stream-flow str def)
  (let ([next (stream-rest str)])
    (if (stream-empty? next)
      (list def)
      next)))


