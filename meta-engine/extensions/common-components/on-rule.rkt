#lang racket

(provide on-rule)

(require "../../core/main.rkt")

;(define-syntax-rule (on-rule condition updater [else (get-value (CURRENT-COMPONENT))])
;  (if condition
;      updater
;      else
;      ))

(define-syntax on-rule
  (syntax-rules ()
    [(on-rule condition updater else) (if condition updater else)]
    [(on-rule condition updater) (on-rule condition updater (get-value (CURRENT-COMPONENT)))]))