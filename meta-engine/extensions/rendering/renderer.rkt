#lang racket

(provide play play!
         CURRENT-WIDTH CURRENT-HEIGHT
         center-posn
         register-fonts!
         get-sprite-width
         get-sprite-height
         init-db)

(require racket/match
         racket/fixnum
         racket/flonum
         lux

         (prefix-in ml: mode-lambda)
         (prefix-in ml: mode-lambda/static)
         (prefix-in gl: mode-lambda/backend/gl)

         (prefix-in ml: mode-lambda/text/runtime)
         ;Don't include this here.  It causes racket/gui/base to be included -- which causes Travis to break due to not having a display (No display :0 error).
         ;(prefix-in ml: mode-lambda/text/static)
         lux/chaos/gui/key
         lux/chaos/gui/mouse
         )

(require "../../core/main.rkt"
         "../common-components/main.rkt" 
         "./animated-sprite.rkt"
         image-coloring)

(provide buttons
         mouse-input
         key->char)

;TODO: This needs to get fleshed out better.  And probably needs to get handled in extensions/input/
;  so that we can better separate rendering from input.

(define (key->char key)
  (cond [(char? key) key]
        [(symbol? key) (read (open-input-string (~a "#\\" key)))]
        [(string? key) (read (open-input-string (~a "#\\" key)))]
        [else (error "That wasn't a valid key!")]))

(define (key-code->symbol a-key)
  (define key-str (~a a-key))
  (cond [(string=? key-str "\b") 'backspace]
        [(string=? key-str "\n") 'enter]
        [(string=? key-str "\r") 'enter]
        [(string=? key-str " ")  'space]
        [else (string->symbol key-str)]))

(define-syntax-rule (define-all-keys buttons keys ...)
  (begin (provide buttons)
         (define buttons
           (make-hash (list (cons 'keys #f)
                            ...)))))

(define-syntax-rule (make-key-hash keys ...)
  (make-hash (list (cons (key->char 'keys) #f)
                   ...)))

(define-all-keys buttons
  left right up down wheel-up wheel-down
  rshift lshift
  backspace
  enter
  space
  a b c d e f g h i j k l m n o p q r s t u v w x y z
  A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
  0 1 2 3 4 5 6 7 8 9
  * - / + = < >
  |#| : |,| |.| | |
  |"| |'|
  |]| |[|
  )
  
#;(define buttons
  (hash
    #\a #f
    #\d #f
    #\w #f
    #\s #f

    #\space #f))

(define mouse-input
  (make-weak-hash
   (list (cons 'left #f)
         (cons 'right #f)
         (cons 'position (posn 0 0)))))


(define (center-posn)
  (posn (/ CURRENT-WIDTH 2) (/ CURRENT-HEIGHT 2)))


(struct game+render ;TODO: CHANGE THIS NAME
  ( state render-tick)
  #:methods gen:word
  [(define (word-fps w)
     60.0)  
   (define (word-label s ft)
     (lux-standard-label "Values" ft))
   
   (define (word-output w)
     (match-define (game+render state render-tick) w)
     (render-tick state))

   (define (word-event w e)
     (cond
       [(or (eq? e 'close)
            (and (key-event? e)
                 (eq? (send e get-key-code) 'escape)))
        (begin
          (display-performance-stats)
          #f)
        ]
       
       [(and (key-event? e)
             (eq? 'press 
                  (key-code->symbol (send e get-key-release-code))))
        (begin
            (hash-set! buttons
                      (key-code->symbol (send e get-key-code))
                      #t)
          w)]

       [(and (key-event? e)
             (eq? 'release
               (key-code->symbol (send e get-key-code))))
        (begin
            (hash-set! buttons
                      (key-code->symbol (send e get-key-release-code))
                      #f)
          w)]
       
       [(and (mouse-event? e)
             (send e moving?))
        (let-values ([(mouse-x mouse-y) (mouse-event-xy e)])
          (hash-set! mouse-input
                     'position (posn mouse-x mouse-y))
          w)]

       [(and (mouse-event? e)
             (send e button-changed?)
             (send e button-down?))
        (begin
          (hash-set! mouse-input
                     (string->symbol (string-trim (~a (send e get-event-type)) "-down"))
                     #t)
          w)]

       [(and (mouse-event? e)
             (send e button-changed?)
             (send e button-up?))
        (begin
          (hash-set! mouse-input
                     (string->symbol (string-trim (~a (send e get-event-type)) "-up"))
                     #f)
          w)]
       
       [else w]
       ))
   
   (define (word-tick w)
     (match-define (game+render state render-tick) w)
     (define new-state (tick state))

     (game+render new-state 
                  render-tick))])

(define (get-gui #:width [w 480] #:height [h 360])
  (define make-gui (dynamic-require 'lux/chaos/gui 'make-gui))
  (make-gui #:start-fullscreen? #f
              #:frame-style (if (eq? (system-type 'os) 'windows)
                                (list 'no-resize-border
                                      'no-caption)
                                (list 'no-resize-border) ;DON'T CHANGE THIS
                                )
              #:mode gl:gui-mode
              #:width w
              #:height h))


(define recompiled #f)
(define should-recompile? #f)
(define csd #f)
(define CURRENT-WIDTH 400)
(define CURRENT-HEIGHT 400)

(define (init-db)
  (define sd  (ml:make-sprite-db))
  (define to-compile (get-queued-sprites))

  (for ([i to-compile])
    (add-sprite! sd (first i) (second i)))

  (set! recompiled #t)

  (define ml:load-font! (dynamic-require 'mode-lambda/text/static 'load-font!))

  ; set the mode lambda font
  (set! game-fonts
        (map (λ(f)
               (struct-copy font f
                            [ml:font
                             (ml:load-font! sd
                                            #:scaling 1.0 ;(get-backing-scale)
                                            #:size (font-size f)
                                            #:face   (font-face f)
                                            #:family (font-family f)
                                            #:style  (font-style f)
                                            #:weight (font-weight f)
                                            #:underlined? (font-underline? f)
                                            ;#:smoothing 'unsmoothed
                                            )]))
             game-fonts))
  
  (set! csd (ml:compile-sprite-db sd))

  ; set the mode lambda text renderer
  (set! game-fonts
               (map (λ(f)
                      (struct-copy font f
                                   [renderer (ml:make-text-renderer (font-ml:font f) csd)]))
                    game-fonts))
  
  csd)

(define (maybe-init-db)
  (when (or new-sprites-added should-recompile?)
    (set! should-recompile? #f)
    (displayln "Recompiling sprite db")
    (init-db) ;Try reinitializing the database, pulling from the sprite queue again
    ))

(define (play! #:width (W 400) 
               #:height (H 400)
               g)
  (mutable! (play #:width W #:height H g)))

(define (play #:width (W 400) 
              #:height (H 400)
              g)

  (set! CURRENT-WIDTH W)
  (set! CURRENT-HEIGHT H)

  (define render-tick (get-mode-lambda-render-tick W H))

  (call-with-chaos
   (get-gui #:width W #:height H)
   (λ () (fiat-lux 
            (game+render g render-tick))))) 

(define (new-layer w h)
  (ml:layer (real->double-flonum w)
            (real->double-flonum h)))


(define (get-mode-lambda-render-tick W H)
  (define W/2 (/ W 2))
  (define H/2 (/ H 2))

  ;Initialize the compiled sprite database
  ;Use the entities, plus their sprites, to determine the initial sprite database

  (define layers (vector
                   (new-layer W/2 H/2)
                   (new-layer W/2 H/2)
                   (new-layer W/2 H/2)
                   (new-layer W/2 H/2)
                   (new-layer W/2 H/2)))

  ;Set up our open gl render function with the current sprite database

  (define ml:render #f)

  ;(init-db) ;Initializes csd

  (define (render-game g)
    (maybe-init-db)

    (set! ml:render 
      (if recompiled
        (begin
          (set! recompiled #f)
          (gl:stage-draw/dc csd W H 8))
        ml:render))

    ;Find uncompiled entities...
    ;Recompile the database if we added anything:
    (define dynamic-sprites 
      (game->ml-sprite-list g))

    (define static-sprites (list))

    ;Actually render them
    (ml:render layers
               static-sprites
               dynamic-sprites))

  render-game)

(require (prefix-in h: 2htdp/image))

; ============ FONT RENDERER ============
(define (register-fonts! . fonts)
  (define (seen-font-before f)
    (findf (curry font-eq? f) game-fonts))

  (define (object->font f)
    (font (send f get-size)
          (send f get-face)
          (send f get-family)
          (send f get-style)
          (send f get-weight)
          (send f get-underlined)
          #f
          #f))
  
  (define uncompiled-fonts
    (filter-not seen-font-before
                fonts))

  (and (not (empty? uncompiled-fonts))
       #;
       (displayln "Registering New Fonts:")
       #;
       (displayln (~a (remove-duplicates (map object->font uncompiled-fonts))))
       (set! game-fonts (append game-fonts (remove-duplicates (map object->font uncompiled-fonts))))
       (set! should-recompile? #t)
       ))

(struct font (size face family style weight underline? ml:font renderer) #:transparent)

(define game-fonts
  (list (font 13.0 MONOSPACE-FONT-FACE
              'modern 'normal 'normal
              #f
              #f
              #f)))

(define (font-eq? f1 f2)
  (define f1-size   (send f1 get-size))
  (define f1-face   (send f1 get-face))
  (define f1-family (send f1 get-family))
  (define f1-style  (send f1 get-style))
  (define f1-weight (send f1 get-weight))
  (define f1-underline? (send f1 get-underlined))

    
  (define f2-size   (font-size f2))
  (define f2-face   (font-face f2))
  (define f2-family (font-family f2))
  (define f2-style  (font-style f2))
  (define f2-weight (font-weight f2))
  (define f2-underline? (font-underline? f2))
    
  (and (= f1-size f2-size)
       (equal? f1-face f2-face)
       (eq? f1-family f2-family)
       (eq? f1-style f2-style)
       (eq? f1-weight f2-weight)
       (eq? f1-underline? f2-underline?)
       ))

(define (text-sprite->ml:sprite ts e)
  ;(define get-backing-scale (dynamic-require 'racket/gui/base 'get-display-backing-scale))
  (define (get-backing-scale) 1.0)
  (define ts-string (text-sprite-string ts))
  (define ts-scale (text-sprite-scale ts))
  (define ts-font (text-sprite-font ts))
  (define ts-color (text-sprite-color ts))
  
  (define ts-font-size (get-font-size ts))
    
  (define text-renderer
    (font-renderer
     (or (and ts-font
              (findf (curry font-eq? ts-font) game-fonts))
         (first game-fonts))))

  (define (ensure-color ts-color)
    (cond [((or/c symbol? string?) ts-color) (name->color ts-color)]
          [(color? ts-color) ts-color]
          [else (error "That wasn't a symbol, string, or color!")]))

  (define p (call-if-proc (get-position e (posn 0 0))))
  
  (and text-renderer
       (let ([c (ensure-color ts-color)]
             [x-scale (posn-x (call-if-proc (get-size-xy e (posn (get-size e 1) (get-size e 1)))))]
             [y-scale (posn-y (call-if-proc (get-size-xy e (posn (get-size e 1) (get-size e 1)))))])
         (text-renderer ts-string
                        #:r (color-red c) #:g (color-green c) #:b (color-blue c)
                        #:layer (call-if-proc (get-layer e 0))
                        
                        #:mx (real->double-flonum (* x-scale ts-scale (get-backing-scale)))
                        #:my (real->double-flonum (* y-scale ts-scale (get-backing-scale)))
                        #:a (real->double-flonum (call-if-proc (get-transparency e 1)))
                        (real->double-flonum (* (get-backing-scale)
                                                (posn-x p)))
                        (real->double-flonum (* (get-backing-scale)
                                                (+ (posn-y p)
                                                   (- (* ts-font-size .75)))))
                        )))
  )

(define (game->ml-sprite-list g)
  (define ret '())

  (for ([e (game-entities g)])
    (define s (get-component e 'sprite))

    (when (and s (get-value s))
      (define eid (entity-id e))

      (maybe-init-db)

      (define s-val (call-if-proc (get-sprite s)))
      
      (define sid 
        (and (image-sprite? s-val) ;(symbol? s-val)
             (ml:sprite-idx csd (image-sprite-id s-val))))


      ;Note.  Where there are many entities (several hundred), all of these get-*s add up.  Consider looking for optimizations.  (However, it is also worth noting that rendering is usually not the first bottleneck.  Only after certain optimizations -- e.g. demos/bullet-cloud.rkt -- does rendering become the bottleneck)
      (define p (call-if-proc (get-position e (posn 0 0))))

      (define mls
        (cond [(text-sprite? s-val) (text-sprite->ml:sprite s-val e)]
              [(image-sprite? s-val) (ml:sprite #:layer (call-if-proc (get-layer e 0))
                                        #:m (real->double-flonum (call-if-proc (get-size e 1)))
                                        #:mx (real->double-flonum (posn-x (call-if-proc (get-size-xy e (posn (get-size e 1) (get-size e 1))))))
                                        #:my (real->double-flonum (posn-y (call-if-proc (get-size-xy e (posn (get-size e 1) (get-size e 1))))))
                                        #:theta (real->double-flonum (call-if-proc (get-rotation e 0)))
                                        #:a (real->double-flonum (call-if-proc (get-transparency e 1))) 
                                        (real->double-flonum (posn-x p))
                                        (real->double-flonum (posn-y p))
                                        sid)]
              [else (error "That wasn't a valid sprite")]))

      (set! ret (cons mls ret))) 
    

    (define a
      (get-component e 'also-render))

    (when a
     (set! ret (append (game->ml-sprite-list (get-value a)) ret))  

      ))


  ret
  

  )

(define (call-if-proc p)
  (if (procedure? p) (p) p))


(require 2htdp/image)

;Adds to an uncompiled sprite database...
(define (add-sprite! db id-sym i)
  (ml:add-sprite!/value db id-sym i))



