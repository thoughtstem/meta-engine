#lang racket

(provide counter get-counter set-counter
         normal-counter
         number-stream get-number-stream)
(require "../../core/main.rkt")

(define-component counter number?)

(define-component number-stream stream?)

(define (normal-counter)
  (counter 0 (^ add1)))
