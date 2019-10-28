#lang racket

(provide make-text)

(require "../../core/main.rkt")
(require "../rendering/renderer.rkt")
(require "../rendering/animated-sprite.rkt")
(require 2htdp/image
         (only-in racket/draw make-font))

(define-syntax-rule (image-generator name img)
  (begin
    (provide name)

    (define temp #f) 
    (define (name)

      (when (not temp)
        (set! temp (register-sprite img)))

      temp)))

(image-generator dot (circle 1 'solid 'black))
(image-generator red-dot (circle 5 'solid 'red))
(image-generator rot-arrow 
                 (above
                   (triangle 5 'solid 'green)
                   (circle 1 'solid 'black)
                   (square 5 'solid 'red)))

; ==== TEXT SPRITES ====
(define (make-text str
              #:scale       [scale 1]
              #:font-size   [f-size 13]
              #:font-face   [f-face MONOSPACE-FONT-FACE]
              #:font-family [f-family 'modern]
              #:font-style  [f-style  'normal]
              #:font-weight [f-weight 'normal]
              #:color       [color 'yellow]
              ;#:blink-color [b-color 'red]
              ;#:mode        [mode 'normal]
              ;#:delay       [delay 20]
              )
  (define new-font (make-font #:size   f-size
                              #:face   f-face
                              #:family f-family
                              #:style  f-style
                              #:weight f-weight))
  (register-fonts! new-font)
  (text-sprite str #:scale scale #:font new-font #:color color))

