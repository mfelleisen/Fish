#lang info
(define collection "Fish")
(define pkg-desc "derived from the source for the Fall 2020 Sw Dev project")
(define pkg-authors '(matthias))
(define version "1.0")

(define sw-dev "git://github.com/mfelleisen/SwDev.git")

(define compile-omit-paths
  '("Old"
     "debugging-aid.rkt"))

(define deps
  `("base"
     "net-lib"
     "pict-lib"
     "scribble-lib"
     "typed-racket-lib"
     "scribble-abbrevs"
     "htdp-lib"
     "gregor-lib"
     "gui-lib"
     "trace-contract"
     "racket-doc"
     "profile-lib"
     "rackunit-lib"
     ,sw-dev))

(define build-deps
  `( ,sw-dev
      "gui-lib"
      "data-enumerate-lib"
      "at-exp-lib" 
      "rackunit-lib"))

(define scribblings '(("scribblings/Fish.scrbl" ())))
