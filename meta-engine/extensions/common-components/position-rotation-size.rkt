#lang racket

(provide
  x
  y

  to

  position
  set-position
  get-position
  position?

  rotation
  set-rotation
  get-rotation
  rotation?

  destination
  set-destination
  get-destination
  destination?

  size
  set-size
  get-size
  size?

  move-to
 
  posn-map
  posn-wrap
  posn-modulo
  
  posn-magnitude
  posn-dist
  posn-normalize

  move-to-destination


  )

(require "../../core/main.rkt" posn)

(define-component position posn?)
(define-component rotation number?)
(define-component size number?)                                         
(define (posn-map f p)
  (posn (f (posn-x p))
        (f (posn-y p))))

(define (posn-modulo p m)
  (posn-map
    (curryr modulo m)  
    p))

(define (posn-wrap b t p)
  (posn-map
    (curry number-wrap b t)
    p))

(define (number-wrap b t n)
  ;Works for most cases, but doesn't handle if n is more than (- t b) away from t or b.
  (cond 
    [(< n b) (- t (- b n))]
    [(> n t) (+ b (- n t))]
    [else n]))

(define (x e)
  (posn-x (get-position e)))

(define (y e)
  (posn-y (get-position e)))

(define (move-to p e)

  (define current-p 
    (get-component e 'position))

  (define new-p
    (set-position current-p p))

  (update-component e 
                    current-p
                    new-p))


(define (posn-magnitude p)
  (sqrt
    (+
      (sqr (posn-x p))
      (sqr (posn-y p)))))

(define (posn-dist p1 p2)
  (posn-magnitude
    (posn-subtract p1 p2)))

(define (posn-normalize p)
  (posn-scale 
    (/ 1 (posn-magnitude p)) 
    p))

(define (there-yet dist)
  (< (posn-dist (get-position)
                (get-destination))
     dist))

(define-component destination posn?)

(define (move-toward by p1 p2)
  (define diff 
    (posn-subtract p2 p1))

  (define dir
   (if (zero? (posn-magnitude diff)) (posn 0 0)
    (posn-scale by
     (posn-normalize
      diff))))
  
  (posn-add dir p1))

(define (move-to-destination speed)
  (if (there-yet speed)
    (get-destination)
    (move-toward speed
      (get-value (CURRENT-COMPONENT))
      (get-destination))))


(define (to target #:by (by 0.1))
  (define curr 
   (get-value (CURRENT-COMPONENT)))

  (define diff (- target curr))

  (define dir (if (positive? diff) 1 -1)) 

  (if (< (abs diff) (abs by))
      target
      (+ (* by dir) 
         curr)))



