#lang racket

;Okay, this is a bit hacky (or clever, depending on your perspective).
;  1) User wraps their game in (reload #:on (this-file-changed) ...)
;  2) This returns a game that functions the same way, but checks for changes in their file.
;  3) On a change detection, it COPIES that file to a new path and calls (dynamic-require ...).  This is a trick to make Racket actually load the module.
;  4) But what if the user was calling (play! ...) in their file?  The hotswapping process twiddles the behaviour of (reload ...) so that it captures the passed-in game (for hotswapping), then throws an error to avoid anything downstream of (reload ...). 

;Current drawbacks.  
;  You can hotswap multiple games but they need to be in separate files.  (see test.rkt)
;  If you hotswap a game that depends on other files, those other modules will not be reloaded.

(provide reload
         file-changed
         this-file)

(require meta-engine/core/main
         meta-engine/extensions/rendering/main
         (for-syntax racket)
         racket/exn)

(define-component swapper void?)

(define last-path (build-path (~a ".hotswap" (random))))

(define (do-swap file)
  (copy-file file last-path #t)
  (displayln "Trying to swap!")

  (parameterize ([should-capture? #t])
    (with-handlers ([exn:fail? 
                      (lambda (e) 
                        (if (string=? (exn-message e)
                                      "hotswap")
                            (displayln "Swapped!") 
                            (begin
                              (displayln "Got an error!") 
                              (displayln (exn->string e))))
                        
                        (delete-file last-path)
                        (set! last-path (build-path (~a ".hotswap" (random))))
                        )])
                   (dynamic-require last-path #f))))

(define (swap-if-necessary swap-to)
  (define should-swap-to (swap-to))
  (when should-swap-to
    (do-swap should-swap-to)))

(define (file-changed file)
  (let () 
    (define time (file-or-directory-modify-seconds file))
    (thunk
      (if (file-exists? file)
        (let () 
          (define updated-time (file-or-directory-modify-seconds file))
          (define should-swap? (not (= time updated-time)))
          (set! time updated-time)
          (if should-swap?
            file
            #f))
        #f))))

(define-syntax (this-file stx)
  (define file (syntax-source stx))
  #`#,file)

(define should-capture? (make-parameter #f))
(define next-g #f)

(define (reload #:on on g)
  (define wrapped-g
    (game
      (entity
        (swapper (void) 
                 (swap-if-necessary on))
        (also-render g
                     (if (and next-g 
                              (not (eq? next-g (get-also-render))))
                       (begin0
                         next-g
                         (set! next-g #f))
                       (tick (get-also-render)))))))

  (if (should-capture?)
    (begin
      (set! next-g wrapped-g) 
      (error "hotswap") ;Prevents downstream play!s from happening...  We catch this in (do-swap ...)
      ) 
    wrapped-g))



