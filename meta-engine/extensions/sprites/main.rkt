#lang at-exp racket

;(provide make-text)

(require "../../core/main.rkt")
(require "../rendering/renderer.rkt")
(require "../rendering/animated-sprite.rkt")
(require 2htdp/image
         ts-kata-util)

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
(define/contract/doc (make-text [str "META-TEXT"]
                                #:scale       [scale 1]
                                #:font-size   [f-size 13]
                                #:font-face   [f-face MONOSPACE-FONT-FACE]
                                #:font-family [f-family 'modern]
                                #:font-style  [f-style  'normal]
                                #:font-weight [f-weight 'normal]
                                #:color       [color 'yellow]
                                #:underlined? [underlined? #f]
                                ;#:blink-color [b-color 'red]
                                ;#:mode        [mode 'normal]
                                ;#:delay       [delay 20]
                                ;". str" todo make it work with a list
                                )

  (->i ()
       ([str string?]
        #:scale       [scale number?]
        #:font-size   [f-size number?]
        #:font-face   [f-face any/c]
        #:font-family [f-family symbol?]
        #:font-style  [f-style  symbol?]
        #:font-weight [f-weight symbol?]
        #:color       [color any/c]
        #:underlined? [underlined? boolean?])
  [returns any/c])

 @{Function to make text.}
  
  (define make-font (dynamic-require 'racket/draw 'make-font))
  (define new-font (make-font #:size   f-size
                              #:face   f-face
                              #:family f-family
                              #:style  f-style
                              #:weight f-weight
                              #:underlined? underlined?))
  (register-fonts! new-font)
  
  ;(define (create-sprite str)
  ;  (text-sprite str #:scale scale #:font new-font #:color color))
  ;(map create-sprite str)
  (text-sprite str #:scale scale #:font new-font #:color color)
  )

