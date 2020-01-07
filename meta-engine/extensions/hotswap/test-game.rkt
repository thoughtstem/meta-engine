#lang racket

(provide g)

(require meta-engine 2htdp/image)

(require "./main.rkt" )

(define red-circle 
  (register-sprite (square 40 'solid 'purple)))

(define g
  (reload 
    #:on (file-changed (this-file))
    (game
      (entity
        (position (posn 250 200))
        (rotation 0 (^ (curry + 0.1)))
        (sprite red-circle))
      (entity
        (position (posn 150 200))
        (rotation 0 (^ (curry + 0.1)))
        (sprite red-circle)))))



