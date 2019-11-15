#lang racket 
(provide (rename-out [make-animation-system
                      animation-system]) 
         get-animation-system
         animated-sprite)

(require "../../core/main.rkt"
         "../input/main.rkt"
         "./animated-sprite.rkt"
         "../common-components/main.rkt"
         "../common-components/time.rkt")

(define-component animation-system entity?)

(define (call-if-proc p)
  (if (procedure? p) (p) p))

(define time-based-counter
  (thunk*
   (+ (get-counter) (get-game-delta-time))))

;Cool.  What's the next abstraction step?
(define (make-animation-system 
          #:counter-update   [counter-update time-based-counter]
          #:direction-update [direction-update identity]
          #:fps              [fps 5]
          up-frames right-frames down-frames left-frames)
  (list
    (animation-system
      (entity 
        (counter 0 (^ counter-update)
                 )
        (direction (posn 0 0) (^ direction-update))

        (facing 'left
                (cond 
                  [(> 0 (posn-x (get-direction)))
                   'left ]
                  [(< 0 (posn-x (get-direction)))
                   'right]
                  [(> 0 (posn-y (get-direction)))
                   'up ]
                  [(< 0 (posn-y (get-direction)))
                   'down]
                  [else (get-facing)]))

        (sprite (first left-frames) 
                (list-ref 
                  (cond 
                    [(eq? 'up (get-facing)) up-frames]
                    [(eq? 'right (get-facing)) right-frames]
                    [(eq? 'left (get-facing)) left-frames]
                    [(eq? 'down (get-facing)) down-frames])
                  (if (equal? (posn 0 0) (get-direction))
                    0  
                    #;(exact-floor (/ (modulo (get-counter) 
                                            (* (length left-frames) (call-if-proc fps)))
                                    (call-if-proc fps)))
                    (modulo (exact-floor (* (get-counter) (call-if-proc fps)))
                            (* (length left-frames)))

                    ))))
      (^ tick-entity))

    (sprite (first left-frames)
            (get-sprite (get-animation-system)))))

(define (animated-sprite 
          #:counter-update   [counter-update time-based-counter]
          #:fps              [fps 5]
          frames)
  (list
   (animation-system
    (entity 
     (counter 0 (^ counter-update))
     (sprite (first frames)
             (list-ref frames
                       (modulo (exact-floor (* (get-counter) (call-if-proc fps)))
                               (* (length frames)))
                       )))
    (^ tick-entity))

    (sprite (first frames)
            (get-sprite (get-animation-system)))))




