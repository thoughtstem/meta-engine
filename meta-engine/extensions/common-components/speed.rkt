#lang racket 

(provide speed
         get-speed
         set-speed)

(require "../../core/main.rkt")

(define-component speed number?)

;Abstract somewhere better?