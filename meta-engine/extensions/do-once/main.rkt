#lang racket

(provide do-once
         do-once-entity 
         do-once-component)

(require "../../core/main.rkt")

(define-component toggle boolean?)
(define-component has-toggled? boolean?)

(define (do-once-entity . components)
  (entity
    (toggle #f #t)
    components
    (has-toggled? #f (get-toggle))))

(define (do-once val (else (get-value (CURRENT-COMPONENT))))
  (if (and (get-toggle)
           (not (get-has-toggled?)))
    val
    else))

(define-component toggle-component component?)

(define-syntax-rule (do-once-component #:named c
                                       components ...)
  (c
    (do-once-entity
      components ...)
    (^ tick-entity)))

