#lang racket

(provide tick2)

(require "./base.rkt"
         "./crud.rkt"
         "./util.rkt"
         "./printer.rkt"
         "./debug.rkt")

(define table #f)

(define (tick2 g)
  (when (not table)
    (set! table (game->table g))) 

  (displayln table)

  g)


(define (game->table g)
  (flatten-game g))

(define the-game (make-parameter #f))
(define the-entity (make-parameter #f))

(define (flatten-game g)
  (parameterize ([the-game g])
    (flatten
      (map flatten-entity (game-entities g)))))

(define (flatten-entity e)
  (define cs (entity-components e)) 

  (flatten
    (map (lambda (c)
           (define v (get-value c))
           (cond 
             [(game? v)
              (flatten-game v)]
             [(entity? v)
              (flatten-entity v)]
             [else 
               (vector (the-game) 
                       e 
                       c)

               ]))
         cs)))

