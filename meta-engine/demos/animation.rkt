#lang racket

; UPDATE - Added framerate independant animation and movement
(provide (rename-out [e elf-avatar]))

(require meta-engine)

(require 2htdp/image)   

(define elf (bitmap "./images/darkelf-sheet.png"))
(define elves-right (sheet->list elf #:row 2))
(define elves-left  (sheet->list elf #:row 1))
(define elves-down  (sheet->list elf #:row 0))
(define elves-up  (sheet->list elf #:row 3))

(define (get-x-speed) 120) ; 100 pps (pixels per second)
(define (get-y-speed) 120)

(define (get-time-based-input-position)
  (define capped-dt (min (get-game-delta-time) (/ 1 30.0)))
  (posn-add (get-position)
            (posn-multiply (as-posn (get-current-input))
                           (posn (*  capped-dt (get-x-speed))
                                 (*  capped-dt (get-y-speed))))))

(define direction-from-input
  (thunk* 
    (as-posn (get-current-input))))

(define e
  (entity
    ;(movement-system 
    ;  #:direction-update direction-from-input)

    (position (posn 200 200)
              (get-time-based-input-position))

    (animation-system
      #:direction-update direction-from-input
      #:fps 12
      elves-up elves-right elves-down elves-left )))

(module+ main
(play! 
    (game
      (delta-time-entity)
      (input-manager) 
      e)))


