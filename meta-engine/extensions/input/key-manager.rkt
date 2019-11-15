#lang racket

(provide key-down?
         key-change-down?
         on-key
         on-key-hold
         key-manager-entity)

(require "../../core/main.rkt")

(require 
  "../rendering/renderer.rkt"
  "../common-components/main.rkt")

(define-component key-hash hash?)
(define-component last-key-hash hash?)



(define (key-down? key)
  (define g (CURRENT-GAME))
  (define key-hash (get-key-hash (get-entity g (has-name 'key-manager))))
  (hash-ref key-hash key))

(define (key-change-down? key)
  (define g (CURRENT-GAME))
  (define key-hash (get-key-hash (get-entity g (has-name 'key-manager))))
  (define last-key-hash (get-last-key-hash (get-entity g (has-name 'key-manager))))
  (and
   (hash-ref key-hash key)
   (not (hash-ref last-key-hash key))))

(define-syntax on-key
  (syntax-rules ()
    [(on-key key updater else) (on-rule (key-change-down? key) updater else)]
    [(on-key key updater)      (on-key key updater (get-value (CURRENT-COMPONENT)))]))

#|(define (on-key-hold key updater [else (get-value (CURRENT-COMPONENT))])
  (on-rule (key-down? key)
           updater
           else))|#

(define-syntax on-key-hold
  (syntax-rules ()
    [(on-key-hold key updater else) (on-rule (key-down? key) updater else)]
    [(on-key-hold key updater)      (on-key-hold key updater (get-value (CURRENT-COMPONENT)))]))


(define (key-manager-entity)
  (entity (name 'key-manager)
          (last-key-hash #f (get-key-hash))
          (key-hash buttons (hash-copy buttons))
          ))
