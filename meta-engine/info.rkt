#lang info

(define collection "meta-engine")

(define scribblings '(("scribblings/main.scrbl" (multi-page))))

(define deps  '("jack-posn"
		"ansi-color"
		"lux"
		"mode-lambda"
		"threading"
		"https://github.com/thoughtstem/racket-chipmunk.git"
                ;added ts-kata-util to define/contract/doc some funcs
                "https://github.com/thoughtstem/TS-GE-Katas.git?path=ts-kata-util"
                "base"
                "jack-ease"
                "image-coloring"))
(define build-deps '("draw-doc"
                     "racket-doc"
                     "scribble-lib"
                     "rackunit-lib"))
