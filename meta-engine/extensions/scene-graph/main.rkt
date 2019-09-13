#lang racket

;TODO: Move to meta-engine

(provide parent-data-entity

         local-rotation
         get-local-rotation
         get-parent-rotation
         
         local-position
         get-local-position
         get-parent-position

         relative-rotation
         relative-position
         relative-size

         get-global-position
         
         children)

(require "../../core/main.rkt")
(require "../common-components/name.rkt")
(require "../common-components/position-rotation-size.rkt")
(require "../rendering/main.rkt")
(require syntax/parse/define)

(define-component local-rotation number? )
(define-component local-size number? )
(define-component local-position posn? )

(define-syntax (relative-size stx)
  (syntax-parse stx
    [(_ start change)
     #'(list
         (local-size start change)
         (size start
               (* (get-local-size)
                  (get 'parent-data 'size))))]  
    [(_ start)
     #'(list
         (local-size start)
         (size start
               (* (get-local-size)
                  (get 'parent-data 'size))))]))

(define-syntax (relative-rotation stx)
  (syntax-parse stx
    [(_ start change)
     #'(list
         (local-rotation start change)
         (rotation start
                   (+ (get-local-rotation)
                      (get 'parent-data 'rotation))))]  
    [(_ start)
     #'(list
         (local-rotation start)
         (rotation start
                   (+ (get-local-rotation)
                      (get 'parent-data 'rotation))))]))

(define-syntax (relative-position stx)
  (syntax-parse stx
    [(_ start change)
     #'(list
         (local-position start change)
         (position start (get-global-position))) 
     ]
    [(_ start)
     #'(list
         (local-position start)
         (position start (get-global-position)))]))

(define (get-global-position)
  (posn-add (get 'parent-data 'position)   
            (posn-scale
              (get-size)
              (rotate-position-around
                (get-local-position) 
                (get 'parent-data 'rotation)))))   


(define (parent-data-entity start-pos 
                            start-rot
                            start-size)
  (entity
    (name 'parent-data)
    start-pos 
    start-rot
    start-size))

(define (rotate-position-around p r)
  (define new-posn
    (posn 
      (- (* (posn-x p) 
            (cos r))
         (* (posn-y p) 
            (sin r))) 
      (+ (* (posn-y p) 
            (cos r))
         (* (posn-x p) 
            (sin r)) )))

  new-posn)

(define (get-parent-rotation)
  (get 'parent-data 'rotation))

(define (get-parent-position)
  (get 'parent-data 'position))

(define (get-parent-size)
  (get 'parent-data 'size))

(define (children . es)
  (also-render
    (game 
      (parent-data-entity
        (position (posn 0 0))
        (rotation 0)
        (size 1))
      es)
    (^ (compose tick propagate-to-child-parent-data))))

(define (propagate-to-child-parent-data g)
  (define f 
    (compose
      (curryr update-entity 
              (has-name 'parent-data)
              (curryr set-size 
                      (get-size)))
      (curryr update-entity 
              (has-name 'parent-data)
              (curryr set-position 
                      (get-position)))
      (curryr update-entity
              (has-name 'parent-data)
              (curryr set-rotation 
                      (get-rotation)))))     
  (f g))



