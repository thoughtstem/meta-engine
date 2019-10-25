#lang racket

(displayln "HI")

(provide delta-time-entity
         get-delta-time
         get-game-delta-time
         time-then
         get-time-then
         delta-time
         get-delta-time)

(require "../../core/main.rkt"
         "./name.rkt")

(define-component time-then number?)
(define-component delta-time number?)

(define (delta-time-entity . cs)
  (entity
    (name 'delta-time-manager)
    (delta-time 0
                (/
                  (if (get-time-then) 
                    (- (current-inexact-milliseconds)  
                       (get-time-then))
                    0)
                  1000)) 

    cs

    (time-then #f (current-inexact-milliseconds))))


(define (get-game-delta-time)
  (get-delta-time (get-entity (CURRENT-GAME)
                              (has-name 'delta-time-manager))))

