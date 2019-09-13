#lang racket

(require "../../core/main.rkt")
(require "../rendering/animated-sprite.rkt")
(require 2htdp/image)

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

