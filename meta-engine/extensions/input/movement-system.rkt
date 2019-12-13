#lang racket

(provide get-movement-system
         (rename-out [make-movement-system
                      movement-system])
         wrap-around-posn
         get-movement-vector
         )

(require "../../core/main.rkt"
         "../common-components/main.rkt"
         "../rendering/renderer.rkt")

(define-component movement-system entity?)

(define (make-movement-system 
          #:direction-update (direction-update identity))
  (list 
    (movement-system
      (entity
        (direction (posn 0 0) 
                   (^ direction-update))
        (position (posn 200 200)
                  (posn-add 
                    (get-position)
                    (get-direction))))
      (^ tick-entity)) ))

(define (wrap-around-posn p)
  (cond [(> (posn-x p) CURRENT-WIDTH) (posn 0 (posn-y p))]
        [(< (posn-x p) 0)             (posn CURRENT-WIDTH (posn-y p))]
        [(> (posn-y p) CURRENT-HEIGHT)(posn (posn-x p) 0)]
        [(< (posn-y p) 0)             (posn (posn-x p) CURRENT-HEIGHT )]
        [else p]))

(define (get-movement-vector)
  (define capped-dt (min (get-game-delta-time) (/ 1 30.0)))
  (posn-add (get-position)
            (posn-multiply (posn capped-dt capped-dt)
                           (posn-rotate-origin-ccw (modulo (get-direction) 360)
                                    (posn (get-speed) 0)))))
