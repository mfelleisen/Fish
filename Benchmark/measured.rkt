#! /bin/sh
#| -*- racket -*-
exec racket -t "$0" -- ${1+"$@"}
|#

#lang racket/base

(require racket/file)
(require racket/list)
(require racket/format)

(define re #px"(\\d+.\\d+)user")

(define (average file)
  (define lines (file->lines file))
  (define title (first lines))
  (define goods (map (λ (ln) (regexp-match re ln)) lines))
  (define times (filter values goods))
  (unless (= (length times) 5) (error 'average "somethigng's wrong, ~e" (length times)))
  (define numbrs (map (compose string->number second) times))
  (list (~r (/ (apply + numbrs) 5) #:precision 1) title))

(average "xstress-no-trace.out")
(average "xstress-load-free.out")
(average "xstress-trace.out")
