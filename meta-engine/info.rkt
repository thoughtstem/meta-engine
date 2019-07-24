#lang info
(define collection 'multi)

(define scribblings '(("scribblings/main.scrbl" (multi-page))))

(define deps  '("jack-posn"
		"ansi-color"
		"lux"
		"mode-lambda"
		"threading"
		"https://github.com/thoughtstem/racket-chipmunk.git#fixes"
                "base"))
(define build-deps '("draw-doc"
                     "racket-doc"
                     "scribble-lib"
                     "rackunit-lib"))
