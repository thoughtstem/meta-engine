#lang racket

(provide bordered-box
         ;go-to-center
         ;go-to-random
         go-to-pos
         go-to-pos-inside
         )

(require "../../core/main.rkt"
         "../common-components/main.rkt"
         "../rendering/main.rkt"
         "../scene-graph/main.rkt")

(require 2htdp/image)

(define (bordered-box [w #f] [h #f]
                      #:position [p #f]
                      #:relative-position [rp #f]
                      #:outer-border-color [outer-border-color 'black]
                      #:border-color       [border-color 'white]
                      #:color              [box-color 'dimgray]
                      #:other-children [other-children '()]
                      )
  (define outer-border-img (square 1 'solid outer-border-color))
  (define inner-border-img (square 1 'solid border-color))
  (define box-img (square 1 'solid box-color))

  (parent (if rp
              (relative-position rp)
              (position (or p (posn 0 0))
                        (or p (go-to-center))))
          (apply children
                 (append (flatten other-children)
                         (list (parent (sprite (register-sprite box-img))
                                       (size-xy (posn 1 1)
                                                (posn (- (or w CURRENT-WIDTH) 6) (- (or h CURRENT-HEIGHT) 6)))
                                       )
                               (parent (sprite (register-sprite inner-border-img))
                                       (size-xy (posn 1 1)
                                                (posn (- (or w CURRENT-WIDTH) 2) (- (or h CURRENT-HEIGHT) 2)))
                                       )
                               (parent (sprite (register-sprite outer-border-img))
                                       (size-xy (posn 1 1)
                                                (posn (or w CURRENT-WIDTH) (or h CURRENT-HEIGHT)))
                                       )
                               )))))


; ==== POSITION ALIGNMENT FUNCTIONS ====

(define (go-to-center)
  (posn (/ CURRENT-WIDTH 2) (/ CURRENT-HEIGHT 2)))

(define alignment? (or/c 'left        'right        'top         'bottom
                         'top-left    'top-right    'bottom-left 'bottom-right
                         'left-center 'right-center 'top-center  'bottom-center
                         'center))

(define (go-to-pos pos #:offset [offset 0] #:posn-offset (posn-offset (posn 0 0)))
  (define WIDTH CURRENT-WIDTH)
  (define HEIGHT CURRENT-HEIGHT)
  (define p (get-position))
  (define pos-x (posn-x p))
  (define pos-y (posn-y p))
  (posn-add
   posn-offset
   (cond
     [(eq? pos 'left)         (posn offset           pos-y)]
     [(eq? pos 'right)        (posn (+ WIDTH offset) pos-y)]
     [(eq? pos 'top)          (posn pos-x            offset)]
     [(eq? pos 'bottom)       (posn pos-x            (+ HEIGHT offset))]
     [(eq? pos 'top-left)     (posn 0                0)]
     [(eq? pos 'top-right)    (posn WIDTH            0)]
     [(eq? pos 'bottom-left)  (posn 0                HEIGHT)]
     [(eq? pos 'bottom-right) (posn WIDTH            HEIGHT)]
     [(eq? pos 'left-center)  (posn offset                (/ HEIGHT 2))]
     [(eq? pos 'right-center) (posn (+ WIDTH offset) (/ HEIGHT 2))]
     [(eq? pos 'top-center)   (posn (/ WIDTH 2)      offset)]
     [(eq? pos 'bottom-center)(posn (/ WIDTH 2)      (+ HEIGHT offset))]
     [(eq? pos 'center)       (posn (/ WIDTH 2)      (/ HEIGHT 2))]))
  )

(define (go-to-pos-inside pos #:offset [offset 0] #:posn-offset (posn-offset (posn 0 0)))
  (define WIDTH CURRENT-WIDTH)
  (define HEIGHT CURRENT-HEIGHT)
  (define p (get-position))
  ;(define as (get-sprite))
  (define hw (/ (get-entity-width) 2))
  (define hh (/ (get-entity-height) 2))
  (define pos-x (posn-x p))
  (define pos-y (posn-y p))
  (posn-add
   posn-offset
   (cond
     [(eq? pos 'left)         (posn (+ offset hw)           pos-y)]
     [(eq? pos 'right)        (posn (+ (- WIDTH hw) offset) pos-y)]
     [(eq? pos 'top)          (posn pos-x                   (+ offset hh))]
     [(eq? pos 'bottom)       (posn pos-x                   (+ (- HEIGHT hh) offset))]
     [(eq? pos 'top-left)     (posn hw                      hh)]
     [(eq? pos 'top-right)    (posn (- WIDTH hw)            hh)]
     [(eq? pos 'bottom-left)  (posn hw                      (- HEIGHT hh))]
     [(eq? pos 'bottom-right) (posn (- WIDTH hw)            (- HEIGHT hh))]
     [(eq? pos 'left-center)  (posn (+ hw offset)           (/ HEIGHT 2))]
     [(eq? pos 'right-center) (posn (+ (- WIDTH hw) offset) (/ HEIGHT 2))]
     [(eq? pos 'top-center)   (posn (/ WIDTH 2)             (+ hh offset))]
     [(eq? pos 'bottom-center)(posn (/ WIDTH 2)             (+ (- HEIGHT hh) offset))])))