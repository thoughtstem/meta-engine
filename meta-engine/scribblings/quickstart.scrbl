#lang scribble/manual
@require[@for-label[racket/base]]

@title{Quickstart}
@author{thoughtstem}

A typical workflow involves constructing a game out of entities (which themselves are constructed out of components).  You then call @racket[play!] on it -- which invokes the mode-lambda-based renderer.

Here's a quick "hello, world"-style example:

@codeblock{
#lang racket

(require meta-engine
         2htdp/image)

(play!
 (game
   (entity
     (rotation 0 (^ (curry + 0.01)))
     (sprite 
       (register-sprite 
          (square 40 'solid 'red))))))
}

The basic building blocks (games, entities, and components) are defined in @racket[meta-engine/core/main] and extensions (including the renderer) are defined in @racket[meta-engine/extensions/main].  

@defproc[(play! [#:width width number?] 
                [#:height height number?] 
                [game game?])
         game?]{
  Begins calling @racket[tick] on the game and renders the result. 

  In the context of being rendered, certain components have special meanings. 
}

@defproc[(sprite [initial-sprite registered-sprite?] [behaviour behaviour?])
         sprite?]{
  A component that tells the renderer what sprite to render for the entity to which this component is attached.
}

@defproc[(rotation [initial-rotation number?] [behaviour behaviour?])
         rotation?]{
  Controls the absolute rotation of the rendered entity's sprite
}

@defproc[(size [initial-size number?] [behaviour behaviour?])
         size?]{
  Controls the absolute size of the rendered entity's sprite
}

@defproc[(position [initial-position posn?] [behaviour behaviour?])
         position?]{
  Controls the absolute position of the rendered entity's sprite
}


@defproc[(relative-rotation [initial-rotation number?] [behaviour behaviour?])
         rotation?]{
  Controls the relative rotation of the rendered entity's sprite.

  Only has an effect if this entity belongs to a game that is a child of another entity. See @racket[parent].
}

@defproc[(relative-size [initial-size number?] [behaviour behaviour?])
         size?]{
  Controls the relative size of the rendered entity's sprite

  Only has an effect if this entity belongs to a game that is a child of another entity. See @racket[parent].
}

@defproc[(relative-position [initial-position posn?] [behaviour behaviour?])
         position?]{
  Controls the relative position of the rendered entity's sprite.  

  Only has an effect if this entity belongs to a game that is a child of another entity.  See @racket[parent].
}


@defproc[(also-render [initial-game game?] [behaviour behaviour?])
         also-render?]{
  For the entity to which this component is attached, the renderer will recursively also render all the entities in @racket[initial-game].   

  A common @racket[behaviour] is to call @racket[tick], so that the child game's behaviours also happen.

  @codeblock{
    (game
     (entity
      (also-render other-game (^ tick))))
  }

  Note that being a child game in this way does not affect the position, rotation, or size of the game's entities.  For that, you must explicitly state that the parent entity is a parent.
}

@defproc[(parent [component component?] ...)
         entity?]{
  Returns an entity that will propagate its position, rotation, and size along to children (which will only take effect if those children have the appropriate components -- e.g. @racket[relative-position], @racket[relative-rotation], etc.)

  Example:

  @codeblock{

   (require meta-engine 2htdp/image)                         
                                                          
   (define (circle-sprite size color)
      (register-sprite (circle size 'solid color))) 

   (define (celestial-body 
          #:start start                                   
          #:rotation-speed speed ;Not orbiting speed!     
          #:color color                                   
          #:size size                                     
          orbiter1 orbiter2)                                       
    (parent                                                 
      (relative-position start)                             
      (rotation 0 (^ (curry + speed)))                      
      (sprite (circle-sprite size color))                   
      (children orbiter1 orbiter2)))

  } 

  Or you can make use of the fact that components are constructed with normal functions.  You can apply @racket[children] to a list of entities to make them all children.

  @codeblock{

   (require meta-engine 2htdp/image)                         
                                                          
   (define (circle-sprite size color)
     (register-sprite (circle size 'solid color))) 

   (define (celestial-body 
          #:start start                                   
          #:rotation-speed speed ;Not orbiting speed!     
          #:color color                                   
          #:size size                                     
          .                                               
          orbiters)                                       
    (parent                                                 
      (relative-position start)                             
      (rotation 0 (^ (curry + speed)))                      
      (sprite (circle-sprite size color))                   
      (apply children orbiters)))

  } 
}
