#lang racket

(provide rev~>
         rev~>>
         (rename-out [rev~>> join]))

(require syntax/parse/define
         threading)
 
         

(define-syntax (rev~> stx)
  (syntax-parse stx
    [(rev thing ...)
     ;(displayln "HERE")
     (define list-of-things (syntax->datum #'(thing ...)))
     (define reversed-things (reverse list-of-things))
     #`(~> #,@reversed-things)]
    ))

(define-syntax (rev~>> stx)
  (syntax-parse stx
    [(rev thing ...)
     ;(displayln "HERE")
     (define list-of-things (syntax->datum #'(thing ...)))
     (define reversed-things (reverse list-of-things))
     #`(~>> #,@(datum->syntax stx reversed-things))]
    ))

(module+ test
  (rev~>>
   add1
   add1
   1))