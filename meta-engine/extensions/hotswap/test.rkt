#lang racket

(require meta-engine 2htdp/image)

(require "./test-game.rkt")
(require "./test-game2.rkt")

;TODO: 
;  Try with play! and (game ...) in separate files.
;  Try in cutscene

;From the perspective of this file, g may or may not be a hotswapping game.  It shouldn't matter.
(play! 
  (game 
    (parent
      (children
        (entity
          (also-render g (^ tick))) 
        (entity
          (also-render g2 (^ tick)))))))
