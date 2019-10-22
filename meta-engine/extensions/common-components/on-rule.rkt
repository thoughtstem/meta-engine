#lang racket

(provide on-rule)

(require "../../core/main.rkt")

(define (on-rule condition updater [else (get-value (CURRENT-COMPONENT))])
  (if condition
      updater
      else
      ))