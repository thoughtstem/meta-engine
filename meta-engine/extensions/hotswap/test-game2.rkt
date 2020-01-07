#lang racket

(provide g2)

(require meta-engine 2htdp/image)

(require "./main.rkt" )

(define green-circle
  (register-sprite (circle 40 'solid 'green)))

(define g2
  (reload 
    #:on (file-changed (this-file))
    (game
      (entity
        (position (posn 200 200))
        (size 2)
        (rotation (/ pi 3))
        (sprite green-circle)))))
