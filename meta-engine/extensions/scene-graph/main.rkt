#lang racket

;TODO: Move to meta-engine

(provide parent-data-entity

         local-rotation
         get-local-rotation
         get-parent-rotation
         
         local-position
         get-local-position
         get-parent-position

         local-size
         relative-rotation
         relative-position
         relative-size

         get-global-position
         
         parent
         children
         propagate-to-child-parent-data

         entity->parent
         )

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
         (local-size start 
                     change)
         (size start
               (* (get-local-size)
                  (get-parent-size))))]
    [(_ start)
     #'(list
         (local-size start)
         (size start
               (* (get-local-size)
                  (get-parent-size))))]))

(define-syntax (relative-rotation stx)
  (syntax-parse stx
    [(_ start change)
     #'(list
         (local-rotation start 
                         change)
         (rotation start
                   (+ (get-local-rotation)
                      (get-parent-rotation))))]
    [(_ start)
     #'(list
         (local-rotation start)
         (rotation start
                   (+ (get-local-rotation)
                      (get-parent-rotation))))]))

(define-syntax (relative-position stx)
  (syntax-parse stx
    [(_ start change)
     #'(list
         (local-position start 
                         change)
         (position start 
                   (get-global-position))) 
     ]
    [(_ start)
     #'(list
         (local-position start)
         (position start 
                   (get-global-position)))]))

(define (get-global-position)
   (posn-add (get-parent-position)
    (rotate-position-around
     (posn-scale (get-size) (get-local-position))
     (get-parent-rotation))))


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
  (with-handlers ([exn:fail? (thunk* 0)])
   (get 'parent-data 'rotation)))

(define (get-parent-position)
  (with-handlers ([exn:fail? (thunk* (posn 0 0))])
   (get 'parent-data 'position)))

(define (get-parent-size)
  (with-handlers ([exn:fail? (thunk* 1)])
   (get 'parent-data 'size)))

(define (children #:tick [t tick] . es)
  (also-render
    (game 
      (parent-data-entity
        (position (posn 0 0))
        (rotation 0)
        (size 1))
      (map entity->parent es))
    (^ (compose t propagate-to-child-parent-data))))

(define (propagate-to-child-parent-data g)
  (define (f g) 
    (define pd 
      (hash-ref (game-entity-hash g)
                'parent-data))

    (define new-pd
      (set-rotation
        (set-position
          (set-size pd 
                    (get-size))
          (get-position))
        (get-rotation)))
    
    (update-entity g pd new-pd))

  (f g))


(define (parent . cs)
 (add-or-replace-components
  (entity
   (relative-rotation 0) 
   (relative-size 1) 
   (relative-position (posn 0 0))) 
  cs))

(define (entity->parent e)
  (maybe-add-components e
                        (list (relative-rotation 0)
                              (relative-size 1)
                              (relative-position (posn 0 0)))))


