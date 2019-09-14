#lang info

(define scribblings '(("scribblings/main.scrbl" (multi-page))))

(define deps  '("jack-posn"
		"ansi-color"
		"lux"
		"mode-lambda"
		"threading"
		"https://github.com/thoughtstem/racket-chipmunk.git#fixes"
                "base"
                "jack-ease"))
(define build-deps '("draw-doc"
                     "racket-doc"
                     "scribble-lib"
                     "rackunit-lib"))
