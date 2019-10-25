#lang scribble/manual
@require[@for-label[racket/base]]

@(require scribble/lp-include)

@title{meta-engine v2}
@author{thoughtstem}

@(include-section "core/main.scrbl")

@section{Experimental}

Here are some patterns for dealing with common stateful things.

Sometimes you want a component to only update once, and then settle to a constant value.  @racket[do-once-entity] wraps up the necessary extra components for storing the state necessary to know when to stop.

@codeblock{
(play!
 (game
  (do-once-entity
   (position (posn 200 200)
    (do-once
     (posn (random 0 400)
      (random 0 400))))
   (sprite (register-sprite (square 40 'solid 'red))))))
}

But that has the drawback that the hidden state toggleing components are like singletons on that entity.  What if you want two different things to happen once -- but be unrelated to each other?

Here's the pattern for that

@codeblock{
  (define-component rotation-randomizer entity?)
  (define-component position-randomizer entity?)

  (play!
    (game
      (entity
        (do-once-component #:named rotation-randomizer
                           (rotation 0
                                     (do-once
                                       (random 360))))
        (do-once-component #:named position-randomizer
                           (position (posn 200 200)
                                     (do-once
                                       (posn (random 0 400)
                                             (random 0 400)))))
        (position (posn 200 200)
                  (get-position (get-position-randomizer)))
        (rotation 0
                  (get-rotation (get-rotation-randomizer)))
        (sprite (register-sprite (square 40 'solid 'red))))))
}
