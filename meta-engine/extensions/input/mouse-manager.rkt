#lang racket

(provide mouse-down?
         mouse-change-down?
         on-mouse
         on-mouse-hold
         mouse-manager-entity)

(require "../../core/main.rkt")

(require 
  "../rendering/renderer.rkt"
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

(define (on-mouse mouse updater [else (get-value (CURRENT-COMPONENT))])
  (on-rule (mouse-change-down? mouse)
           updater
           else))

(define (on-mouse-hold mouse updater [else (get-value (CURRENT-COMPONENT))])
  (on-rule (mouse-down? mouse)
           updater
           else))

(define (mouse-manager-entity)
  (entity (name 'mouse-manager)
          (last-mouse-hash #f (get-mouse-hash))
          (mouse-hash mouse-input (hash-copy mouse-input))
          ))
