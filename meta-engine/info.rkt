#lang info

(define collection "meta-engine")

(define scribblings '(("scribblings/main.scrbl" (multi-page))))

(define deps  '("jack-posn"
		"ansi-color"
		"lux"
		"mode-lambda"
		"threading"
		"https://github.com/thoughtstem/racket-chipmunk.git"
                "base"
                "jack-ease"
                "image-coloring"))
(define build-deps '("draw-doc"
                     "racket-doc"
                     "scribble-lib"
                     "rackunit-lib"))
