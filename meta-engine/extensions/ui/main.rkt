#lang racket

(provide bordered-box
         go-to-center)

(require "../../core/main.rkt"
         "../common-components/main.rkt"
         "../rendering/main.rkt"
         "../scene-graph/main.rkt")

(require 2htdp/image)

(define (go-to-center)
  (posn (/ CURRENT-WIDTH 2) (/ CURRENT-HEIGHT 2)))

(define (bordered-box [w #f] [h #f]
                      #:position [p #f]
                      #:relative-position [rp #f]
                      #:outer-border-color [outer-border-color 'black]
                      #:border-color       [border-color 'white]
                      #:color              [box-color 'dimgray])
  (define outer-border-img (square 1 'solid outer-border-color))
  (define inner-border-img (square 1 'solid border-color))
  (define box-img (square 1 'solid box-color))

  (parent (if rp
              (relative-position rp)
              (position (or p (posn 0 0))
                        (or p (go-to-center))))
          (children (parent (sprite (register-sprite box-img))
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
                    )))

