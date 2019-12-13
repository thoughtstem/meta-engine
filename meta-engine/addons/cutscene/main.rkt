#lang racket

(provide (rename-out (make-cutscene cutscene))
         page
         current-page get-current-page set-current-page
         current-page-time
         duration get-duration set-duration)

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
(define-component duration (or/c #f number?))

; ===== PAGE ENTITY =====
(define (page #:width        [w #f]
              #:height       [h #f]
              #:position     [p #f]
              #:relative-position [rp #f]
              #:bg           [bg #f]
              #:bg-color     [bg-color (color 50 50 50)]
              #:border-color [border-color 'white]
              #:duration     [dur #f]
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
         (direction (cond [(eq? mode 'still) 0]
                          [(eq? mode 'scroll-right-from-center) 0]
                          [(eq? mode 'scroll-left-from-center)  180]
                          [(eq? mode 'scroll-down-from-center)  90]
                          [(eq? mode 'scroll-up-from-center)    270]
                          ))
         (speed (if (eq? mode 'still)
                    0
                    100))
         (relative-position pos (get-relative-movement-vector))
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
                       (or p (go-to-pos 'center))
                       )
             (position-hack #t)))
   (counter 0 (^ (curry + (get-game-delta-time))))
   (duration dur)
   (death #f (join (on-key 'enter (despawn))
                   (on-key 'space (despawn))
                   (on-key 'q (despawn))
                   (if dur
                       (on-rule (>= (get-counter) dur) (despawn))
                       (get-death))
                ))
   (apply children (append (list (delta-time-entity))
                           offset-items-list                      
                           (list (bordered-box w h #:relative-position (posn 0 0) #:color bg-color #:border-color border-color))))))
; ===== END OF PAGE ENTITY =====

; ===== CUTSCENE / MULTI-PAGE ENTITY =====

;(define-component cutscene entity?)

(define (propagate-to-child-parent-data-with-page g)
  (define (f g) 
    (define pd 
      (hash-ref (game-entity-hash g)
                'parent-data))

    (define new-pd
      (set-current-page
       (set-rotation
        (set-position
         (set-size pd 
                   (get-size))
          (get-position))
        (get-rotation))
       (get-current-page)))
    
    (update-entity g pd new-pd))

  (f g))


(define-component current-page number?)
(define-component last-page number?)
(define-component game-time number?)
(define-component last-page-time number?)

(define (current-page-time [e (CURRENT-ENTITY)])
    (- (get-game-time e) (get-last-page-time e)))

(define (make-cutscene #:name [n 'cutscene]
                       #:position [p #f]
                       #:show-game-time? [show-game-time? #f]
                       . pages)
  (define (remove-death e)
    (remove-component e (λ(c) (eq? (get-component-name c) 'death))))

  (define (update-position e)
    (if (get-component e (λ(c) (eq? (get-component-name c) 'position-hack)))
        (add-or-replace-components e (relative-position (posn 0 0)))
        e
        ))
           
  (define updated-pages (map (compose update-position
                                      remove-death) pages))

  (define (get-page-duration) (get-duration (list-ref pages (get-current-page))))

  (apply entity
    (filter identity
      (list (name n)
            (game-time 0 (^ (curry + (get-game-delta-time)))
                       ;(/ (current-inexact-milliseconds) 1000) ; why doesn't this work? thunk* causes error too
                       )
            (current-page 0 (join (on-key 'enter (^ add1))
                                  (if (get-page-duration)
                                      (on-rule (>= (current-page-time) (get-page-duration)) ; Handling left over time below!
                                               (^ add1))
                                      (get-current-page))))
            
            (death #f (join (on-key 'backspace (despawn))
                            (on-key 'q (despawn))
                            (on-rule (>= (get-current-page) (length pages)) (despawn))))
            
            (last-page-time 0 (if (not (eq? (get-current-page) (get-last-page)))
                                  (if (get-duration (list-ref pages (get-last-page)))
                                      (- (get-game-time) (- (current-page-time)                               ; correcting last-page-time by SUBTRACTING the left over not ADDING!
                                                            (get-duration (list-ref pages (get-last-page))))) ;gets duration BEFORE the switch
                                      (get-game-time))
                                  (get-last-page-time)))
            (position (or p (posn 0 0))
                      (or p (go-to-pos 'center)))
            (relative-rotation 0)
            (relative-size 1)
            (if show-game-time?
                (sprite (make-text "0 second(s)" #:font-size 24)
                        (make-text (~a (~r (get-game-time)
                                           #:precision '(= 3))
                                       " second(s)")
                                   #:font-size 24))
                #f)
            (also-render
             (game)
             (tick
              (propagate-to-child-parent-data-with-page
               (game (delta-time-entity)
                     (key-manager-entity)
                     (entity (name 'parent-data)
                             (current-page 0)
                             (position (or p (posn 0 0)))
                             (rotation 0)
                             (size 1))
                     (list-ref updated-pages (min (get-current-page) (sub1 (length pages))))))))

            (last-page 0 (get-current-page))
          
            ))))