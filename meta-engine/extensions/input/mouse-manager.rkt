#lang racket

(provide mouse-down?
         mouse-change-down?
         on-mouse
         on-mouse-hold
         mouse-manager-entity
         get-mouse-position
         on-sprite-click)

(require "../../core/main.rkt")

(require 
  "../rendering/renderer.rkt"
  "../rendering/animated-sprite.rkt"
  "../common-components/main.rkt")

(define-component mouse-hash hash?)
(define-component last-mouse-hash hash?)

(define (mouse-down? mouse)
  (define g (CURRENT-GAME))
  (define mouse-hash (get-mouse-hash (get-entity g (has-name 'mouse-manager))))
  (hash-ref mouse-hash mouse))

(define (mouse-change-down? mouse)
  (define g (CURRENT-GAME))
  (define mouse-hash (get-mouse-hash (get-entity g (has-name 'mouse-manager))))
  (define last-mouse-hash (get-last-mouse-hash (get-entity g (has-name 'mouse-manager))))
  (and
   (hash-ref mouse-hash mouse)
   (not (hash-ref last-mouse-hash mouse))))

#|(define (on-mouse mouse updater [else (get-value (CURRENT-COMPONENT))])
  (on-rule (mouse-change-down? mouse)
           updater
           else))|#

(define-syntax on-mouse
  (syntax-rules ()
    [(on-mouse button updater else) (on-rule (mouse-change-down? button) updater else)]
    [(on-mouse button updater)      (on-mouse button updater (get-value (CURRENT-COMPONENT)))]))

#|(define (on-mouse-hold mouse updater [else (get-value (CURRENT-COMPONENT))])
  (on-rule (mouse-down? mouse)
           updater
           else))|#

(define-syntax on-mouse-hold
  (syntax-rules ()
    [(on-mouse-hold button updater else) (on-rule (mouse-down? button) updater else)]
    [(on-mouse-hold button updater)      (on-mouse-hold button updater (get-value (CURRENT-COMPONENT)))]))

(define (mouse-manager-entity)
  (entity (name 'mouse-manager)
          (last-mouse-hash #f (get-mouse-hash))
          (mouse-hash mouse-input (hash-copy mouse-input))
          ))

(define (get-mouse-position [e (CURRENT-ENTITY)])
  (define g (CURRENT-GAME))
  (define mouse-hash (get-mouse-hash (get-entity g (has-name 'mouse-manager))))
  (hash-ref mouse-hash 'position))

(define (touching-pointer? [e (CURRENT-ENTITY)])
  (define pos (get-position e))
  (define m-pos (get-mouse-position e))
  
  (define x (posn-x pos))
  (define y (posn-y pos))
  (define mx (posn-x m-pos))
  (define my (posn-y m-pos))
  (define w (get-entity-width e))
  (define h (get-entity-height e))
  
  (and (> mx (- x (/ w 2)))
       (< mx (+ x (/ w 2)))
       (> my (- y (/ h 2)))
       (< my (+ y (/ h 2)))))

(define-syntax on-sprite-click
  (syntax-rules ()
    [(on-sprite-click updater else) (on-mouse 'left
                                              (on-rule (touching-pointer?)
                                                       updater
                                                       else)
                                              else)]
    [(on-sprite-click updater) (on-sprite-click updater (get-value (CURRENT-COMPONENT)))]))
