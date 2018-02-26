#lang racket

(require "../../game-engine.rkt"
         "../common/instructions.rkt"
         "../common/game-over-screen.rkt"
         "../assets/ore-sprite.rkt"
         "../assets/spaceship-sprite.rkt"
         "../assets/space-bg-generator.rkt")

(define WIDTH  640)
(define HEIGHT 480)

(define bg-entity
  (sprite->entity (space-bg-sprite WIDTH HEIGHT 100)
                  #:name     "bg"
                  #:position (posn 0 0)))

(define (spaceship-entity)
  (sprite->entity spaceship-sprite
                  #:name       "ship"
                  #:position   (posn 100 100)
                  #:components (key-movement 5)
                               (on-collide "ore"    increase-speed)))

;Not clear either...  Move or simplify with better API
(define (increase-speed g e)
  (define increase (lambda (k)
                     (key-movement (add1 (key-movement-speed k)))))
  (update-entity e key-movement? increase))


(define (ore-entity p)
  (sprite->entity (ore-sprite (random 10))
                  #:position   p
                  #:name       "ore"
                  #:components (on-collide "ship" randomly-relocate-me)
                                ))

;Could move this to AI?
(define (randomly-relocate-me g e)
  (ore-entity (posn (random WIDTH)
                    (random HEIGHT))))

(define (lost? g e)
  (define names (map get-name (game-entities g)))
  (not (member "ship" names))) ;Nope this is NOT CLEAR

(define (won? g e) ;Nor is this...
  (define              (name-is s e) (string=? s (get-name e)))
  (define ship         (findf (curry name-is "ship") (game-entities g)))
  (define speed        (key-movement-speed (get-component ship key-movement?)))
  (>= speed 10))


(start-game (instructions WIDTH HEIGHT "Use arrow keys to move")
            (game-over-screen won? lost?)
            (ore-entity (posn 200 200))
            (spaceship-entity)
            bg-entity)
 
  