#lang racket

(provide play play!
         CURRENT-WIDTH CURRENT-HEIGHT
         center-posn)

(require racket/match
         racket/fixnum
         racket/flonum
         lux

         (prefix-in ml: mode-lambda)
         (prefix-in ml: mode-lambda/static)
         (prefix-in gl: mode-lambda/backend/gl)
         (prefix-in ml: mode-lambda/text/runtime)
         lux/chaos/gui/key
         lux/chaos/gui/mouse)

(require "../../core/main.rkt"
         "../common-components/main.rkt" 
         "./animated-sprite.rkt")

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
                  (string->symbol (~a (send e get-key-release-code)))))
        (begin
            (hash-set! buttons
                      (string->symbol (~a (send e get-key-code)))
                      #t)
          w)]

       [(and (key-event? e)
             (eq? 'release
               (string->symbol (~a (send e get-key-code)))))
        (begin
            (hash-set! buttons
                      (string->symbol (~a (send e get-key-release-code)))
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
(define csd #f)
(define CURRENT-WIDTH 400)
(define CURRENT-HEIGHT 400)

(define (init-db)
  (define sd  (ml:make-sprite-db))
  (define to-compile (get-queued-sprites))

  (for ([i to-compile])
    (add-sprite! sd (first i) (second i)))

  #;
  (flush-queued-sprites!)

  (set! recompiled #t)
  (set! csd (ml:compile-sprite-db sd))
  csd)

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

  (init-db) ;Initializes csd

  (define (render-game g)
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


(define (game->ml-sprite-list g)
  (define ret '())

  (for ([e (game-entities g)])
    (define s (get-component e 'sprite))

    (when s 
      (define eid (entity-id e))

      (when new-sprites-added 
        (displayln "Recompiling sprite db")
        (init-db) ;Try reinitializing the database, pulling from the sprite queue again
        )

      (define sid 
        (ml:sprite-idx csd (call-if-proc (sprite-id s))))


      ;Note.  Where there are many entities (several hundred), all of these get-*s add up.  Consider looking for optimizations.  (However, it is also worth noting that rendering is usually not the first bottleneck.  Only after certain optimizations -- e.g. demos/bullet-cloud.rkt -- does rendering become the bottleneck)
      (define p (call-if-proc (get-position e (posn 0 0))))

      (define mls
        (ml:sprite #:layer (call-if-proc (get-layer e 0))
                   #:m (real->double-flonum (call-if-proc (get-size e 1)))
                   #:mx (real->double-flonum (posn-x (call-if-proc (get-size-xy e (posn (get-size e 1) (get-size e 1))))))
                   #:my (real->double-flonum (posn-y (call-if-proc (get-size-xy e (posn (get-size e 1) (get-size e 1))))))
                   #:theta (real->double-flonum (call-if-proc (get-rotation e 0)))
                   #:a (real->double-flonum (call-if-proc (get-transparency e 1))) 
                   (real->double-flonum (posn-x p))
                   (real->double-flonum (posn-y p))
                   sid))

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

