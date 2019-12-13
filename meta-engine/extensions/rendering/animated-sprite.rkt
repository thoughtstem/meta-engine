#lang racket

(provide 
  sprite
  sprite?
  sprite-id
  get-sprite
  register-sprite
  new-sprites-added

  get-queued-sprites
  flush-queued-sprites!

  set-insertion-queue!
  set-seen-sprite-ids! 

  sheet->list
  sprite-row->list

  also-render?
  also-render
  get-also-render
  
  transparency
  transparency?
  get-transparency

  layer
  get-layer
  layer?

  (except-out (struct-out image-sprite) image-sprite)
  (rename-out [make-image-sprite image-sprite])
  
  (except-out (struct-out text-sprite) text-sprite)
  (rename-out [make-text-sprite text-sprite])

  set-text-sprite-scale
  set-text-sprite-font
  set-text-sprite-color
  get-font-size
  MONOSPACE-FONT-FACE
  get-sprite-width
  get-sprite-height

  get-entity-width
  get-entity-height
  )

(require "../../core/main.rkt"
         "../common-components/main.rkt"
         (only-in 2htdp/image bitmap/file image->color-list crop image-width image-height text/font))

; ==== IMAGE SPRITE ====
(struct image-sprite (id width height))

(define (make-image-sprite s)
  (if (image-sprite? s)
      s
      (register-sprite s)))

(define (get-sprite-width s)
  (if (text-sprite? s)
      (text-sprite-width s)
      (image-sprite-width s)))

(define (get-sprite-height s)
  (if (text-sprite? s)
      (text-sprite-height s)
      (image-sprite-height s)))

(define (get-scale-posn e)
  (get-size-xy e (posn (get-size e 1)
                       (get-size e 1))))

(define (get-entity-width [e (CURRENT-ENTITY)])
  (if (has-component-named 'sprite e)
      (max (* (get-sprite-width (get-sprite e)) (posn-x (get-scale-posn e)))
           (if (has-component-named 'also-render e)
               (apply max (let ([width-list (map get-entity-width (game-entities (tick (get-also-render e))))])
                            (if (empty? width-list)
                                (list 0)
                                width-list)))
               0))
      (if (has-component-named 'also-render e)
          (apply max (let ([width-list (map get-entity-width (game-entities (tick (get-also-render e))))])
                            (if (empty? width-list)
                                (list 0)
                                width-list)))
          0)))

(define (get-entity-height [e (CURRENT-ENTITY)])
  (if (has-component-named 'sprite e)
      (max (* (get-sprite-height (get-sprite e)) (posn-y (get-scale-posn e)))
           (if (has-component-named 'also-render e)
               (apply max (let ([height-list (map get-entity-height (game-entities (tick (get-also-render e))))])
                            (if (empty? height-list)
                                (list 0)
                                height-list)))
               0))
      (if (has-component-named 'also-render e)
          (apply max (let ([height-list (map get-entity-height (game-entities (tick (get-also-render e))))])
                       (if (empty? height-list)
                           (list 0)
                           height-list)))
          0)))

                                
; ==== TEXT SPRITE ====
(define MONOSPACE-FONT-FACE
  (cond [(eq? (system-type 'os) 'windows) "Consolas" ]
        [(eq? (system-type 'os) 'macosx)  "Menlo"]
        [(eq? (system-type 'os) 'unix)    "DejaVu Sans Mono"]))

(struct text-sprite (string scale font color))

(define (make-text-sprite s
                         #:scale [scale 1]
                         #:font  [font #f]
                         #:color [color 'yellow])
  (text-sprite s scale font color))

(define (text-sprite-width ts)
  (image-width (if (text-sprite-font ts)
                   (let ([f (text-sprite-font ts)])
                     (text/font (text-sprite-string ts)
                              (send f get-size)
                              (text-sprite-color ts)
                              (send f get-face)
                              (send f get-family)
                              (send f get-style)
                              (send f get-weight)
                              #f))
                   (text/font (text-sprite-string ts)
                              13
                              (text-sprite-color ts)
                              MONOSPACE-FONT-FACE
                              'modern
                              'normal
                              'normal
                              #f))))

(define (text-sprite-height ts)
  #;(image-height (if (text-sprite-font ts)
                    (let ([f (text-sprite-font ts)])
                      (text/font (text-sprite-string ts)
                                 (send f get-size)
                                 (text-sprite-color ts)
                                 (send f get-face)
                                 (send f get-family)
                                 (send f get-style)
                                 (send f get-weight)
                                 #f))
                    (text/font (text-sprite-string ts)
                               13
                               (text-sprite-color ts)
                               MONOSPACE-FONT-FACE
                               'modern
                               'normal
                               'normal
                               #f)))
  (if (text-sprite-font ts)
      (send (text-sprite-font ts) get-size)
      13)
  )

(define (set-text-sprite-scale s tf)
  (struct-copy text-sprite tf
               [scale s]))

(define (set-text-sprite-font f tf)
  (struct-copy text-sprite tf
               [font f]))

(define (set-text-sprite-color c tf)
  (struct-copy text-sprite tf
               [color c]))

(define (get-font-size tf)
  (if (text-sprite-font tf)
      (send (text-sprite-font tf) get-size)
      13))



; ==== END TEXT SPRITE ====
 
(define-component also-render game?)
(define-component sprite (or/c text-sprite? image-sprite?))
(define-component layer number?)
(define-component transparency number?)

(define (sprite-id s)
  (get-sprite s))

;Whenever you construct a new sprite, it ends up in the
; insertion queue, along with its id.  This is the last step
; before we throw the image away (to the gcard) and refer to it only by id.
(define insertion-queue '())  ;Holds images, Gets emptied
(define seen-sprite-ids '())  ;Doesn't hold images, Doesn't get emptied

(define (seen? i)
  (member i seen-sprite-ids))

(define (image->id i)
  (string->symbol
    (~a "sprite-"
        (equal-hash-code (~a (image->color-list i))))))

(define new-sprites-added #f)

(define (register-sprite i)
  (let ([id 
          (if (path? i)     
            (string->symbol (~a "sprite-" i))
            (image->id i))]
        [final-image
          (if (path? i)
            (bitmap/file i) 
            i)])

    (when (not (seen? id))
      (set! new-sprites-added #t)

      (set-insertion-queue! (cons (list id final-image) insertion-queue))
      (set-seen-sprite-ids! (cons id seen-sprite-ids)))

    (image-sprite id (image-width final-image) (image-height final-image))))

(define (set-insertion-queue! l)
  (set! insertion-queue l))

(define (set-seen-sprite-ids! l)
  (set! seen-sprite-ids l))

(define (get-queued-sprites) 
  (set! new-sprites-added #f)
  insertion-queue
  
  )

(define (flush-queued-sprites!) 
  (set! insertion-queue '()))


(define (sheet->list i #:row (r 0))
  (define ew (image-width i))
  (define eh (image-height i))

  (define rows 4)
  (define cols 4)

  (define cw  (/ ew cols))
  (define ch  (/ eh rows))

  (define elf1 (register-sprite (crop (* cw 0) (* ch r) cw ch i)))
  (define elf2 (register-sprite (crop (* cw 1) (* ch r) cw ch i)))
  (define elf3 (register-sprite (crop (* cw 2) (* ch r) cw ch i)))
  (define elf4 (register-sprite (crop (* cw 3) (* ch r) cw ch i)))

  (define elves (list elf1 elf2 elf3 elf4))

  elves)

(define (sprite-row->list img columns)
  (define frame-width (/ (image-width img) columns))
  (define frame-height (image-height img))
  (define (get-frame num)
    (crop (* frame-width num) 0 frame-width frame-height img))
  (map (compose make-image-sprite
                get-frame) (range columns)))

