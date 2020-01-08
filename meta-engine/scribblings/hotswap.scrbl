#lang scribble/manual
@require[@for-label[racket/base]]

@title{Hotswapping}
@author{thoughtstem}

Any game can be wrapped up so that it automatically reloads itself whenever its defining file is saved.  This can speed up development considerably.  

Usually, you will want to wrap games in the file where they are defined.  Suppose @racket["g.rkt"] provides @racket[g]:  

@codeblock{
#lang racket
(require meta-engine)

(provide g)

(define red-square 
  (register-sprite (square 40 'solid 'red)))

(define g
  (reload ;We wrap the game here
    #:on (file-changed (this-file))
    (game
      (entity
        (position (posn 250 200))
        (rotation 0 (^ (curry + 0.1)))
        (sprite red-square))
      (entity
        (position (posn 150 200))
        (rotation 0 (^ (curry + 0.1)))
        (sprite red-square)))))
}

When @racket[g] is used elsewhere -- even as a child game within a larger game -- it will reload itself whenever @racket["./g.rkt"] is saved.   

@codeblock{
#lang racket 

(require meta-engine)
(require "./g.rkt")

(play! 
  (game 
    (parent
      (children
        (entity
          (also-render g (^ tick))) ))))
}

If your large game is divided into many smaller games (which it should be), then you can hotswap various parts of it into to main game while you are developing them.  

@section{Odd cases}

Sometimes you'll want to hotswap a game defined in one file, whose various parts are defined in another file.  In cases like that, you'll want to do something like:

@codeblock{
(define g
  (reload 
    #:on (file-changed (build-path "some" "other" "file.rkt"))
    (game
      (some-entity-defined-elsewhere 'green)
      (some-entity-defined-elsewhere 'red))))
}

Now @racket[g] will reload when the other file is changed.  (We currently use this in @tt{#lang cutscene}, where the game object is contructed on the user's behalf "under the hood" but should still be reloaded when the user's file changes.)

@section{Limitations and Implementation Details}

Racket doesn't exactly have support for things like hotswapping a module into a running system.  So this feature is implemented in a way that (although short and sweet) is admittedly a bit hacky.

In essence, when a watched file is changed, the file is copied to a temporary file and @racket[dynamic-require]d.  This loads a brand new module.  But during the loading process, state gets twiddled such that the game passed into @racket[reload] is captured, after which execution is halted.

This allows us to grab the game and hotswap it, without triggering any other calls to @racket[play!] that might be in the file.  (It is, after all, quite common for small games to both define the game and @racket[play!] it in the same file.)

This leads to a known limitation at the moment, you can't (currently) hotswap two games defined in the same file.  We could probably get this to work, but we'd have to make the code more complicated.

The current implementation seems to be relatively flexible and powerful, though.  So we'll leave this for later, if it becomes necessary.






