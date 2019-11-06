#lang racket

(provide (rename-out (make-cutscene cutscene))
         page)

(require "../../extensions/main.rkt")

(require 2htdp/image)

(define (ensure-sprite thing) ;or list or entity
    (cond [(string? thing) (sprite (make-text thing #:font-size 16))]
          [(text-sprite? thing) (sprite thing)]
          [(image-sprite? thing) (sprite thing)]
          ;[(symbol? thing) (sprite thing)]
          [(image? thing) (sprite (register-sprite thing))]
          [(list? thing) thing]
          [(entity? thing) thing]
          [else (error "That wasn't a valid sprite!")]))

(define-component position-hack boolean?)

; ===== PAGE ENTITY =====
(define (page #:width        [w #f]
              #:height       [h #f]
              #:position     [p #f]
              #:relative-position [rp #f]
              #:bg           [bg #f]
              #:bg-color     [bg-color (color 50 50 50)]
              #:border-color [border-color 'white]
              #:duration     [duration #f]
              #:line-padding [line-padding 4]
              #:mode         [mode 'still]
              . items)
  
  (define bg-sprite (if bg
                        (ensure-sprite bg)
                        #f))

  (define sprite-component-list (map ensure-sprite items)) ;inlcudes animation-system (list?) and entity

  (define (get-sprite-or-animation s)
    (if ((or/c list? entity?) s)
        s
        (get-sprite s)))

  (define sprite-list (map get-sprite-or-animation
                           sprite-component-list))
  
  (define (make-line pos s)
    (if (entity? s)
        s
        (entity
         (relative-position pos)
         (if (list? s)
             s
             (sprite s)))))

  (define (get-padded-sprite-height s)
    (cond [(list? s)   (+ line-padding (get-sprite-height (get-sprite (second s))))] ;using height from first frame
          [(entity? s) 0]          ;ignore if it's an entity
          [else        (+ line-padding (get-sprite-height s))]))
    
  (define half-of-total
    (/ (apply + (map get-padded-sprite-height sprite-list)) 2))

  (define offset-items-list
    (flatten (for/list ([item sprite-list]
                        [i (range (length sprite-list))])
               (let ([last-height (apply + (map get-padded-sprite-height (take sprite-list i)))]
                     [half-height-item (/ (get-padded-sprite-height item) 2)])
                 (make-line (posn 0 (+ last-height
                                       half-height-item
                                       (- half-of-total))) item)))))

  (parent
   (if rp
       (relative-position rp)
       (list (position (or p (posn 0 0))
                       (or p (go-to-pos 'center)))
             (position-hack #t)))
   (death #f (join (on-key 'enter (despawn))
                   (on-key 'space (despawn))
                   (on-key 'q (despawn))))
   (apply children (append (list (delta-time-entity))
                           offset-items-list                      
                           (list (bordered-box w h #:relative-position (posn 0 0) #:color bg-color #:border-color border-color))))))
; ===== END OF PAGE ENTITY =====

; ===== CUTSCENE / MULTI-PAGE ENTITY =====

;(define-component cutscene entity?)

(define (propagate-to-child-parent-data-with-counter g)
  (define (f g) 
    (define pd 
      (hash-ref (game-entity-hash g)
                'parent-data))

    (define new-pd
      (set-counter
       (set-rotation
        (set-position
         (set-size pd 
                   (get-size))
          (get-position))
        (get-rotation))
       (get-counter)))
    
    (update-entity g pd new-pd))

  (f g))

(define (make-cutscene #:name [n 'cutscene]
                       #:position [p #f] . pages)
  (define (remove-death e)
    (remove-component e (λ(c) (eq? (get-component-name c) 'death))))

  (define (update-position e)
    (if (get-component e (λ(c) (eq? (get-component-name c) 'position-hack)))
        (add-or-replace-components e (relative-position (posn 0 0)))
        e
        ))
           
  (define updated-pages (map (compose update-position
                                      remove-death) pages))
  
  (entity (name n)
          (counter 0 (on-key 'enter (^ (compose (curryr modulo (length pages))
                                                add1)
                                       )))
          (death #f (join (on-key 'backspace (despawn))
                          (on-key 'q (despawn))
                          (on-key 'enter (on-rule (= (get-counter) (sub1 (length pages))) (despawn)))))
          (position (or p (posn 0 0))
                    (or p (go-to-pos 'center)))
          (relative-rotation 0)
          (relative-size 1)
          (also-render
           (game)
           (tick
            (propagate-to-child-parent-data-with-counter
             (game (delta-time-entity)
                  (key-manager-entity)
                  (entity (name 'parent-data)
                          (counter 0)
                          (position (or p (posn 0 0)))
                          (rotation 0)
                          (size 1))
                  (list-ref updated-pages (get-counter))))))
          )
)