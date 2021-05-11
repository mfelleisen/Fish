#lang racket

(provide
 #; {[Listof X] -> Boolean}
 distinct?
 
 #; {[NEListof X] -> [NEListof X]}
 rotate)

;; -----------------------------------------------------------------------------
(module+ test (require rackunit))
;; -----------------------------------------------------------------------------

(define (rotate l)
  (match l
    [`(,fst ,snd ...) (append snd (list fst))]))

(define distinct?
  (flat-named-contract
   "distinct"
   (lambda (names)
     (let loop ([six names])
       (if (empty? six)
           #t
           (and (not (member (first six) (rest six))) (loop (rest six))))))))

;; -----------------------------------------------------------------------------
(module+ test
  (check-true (distinct? '(a b c)))
  (check-false (distinct? '("a" "b" "c" "a")))

  (check-false (distinct? (list (box 1) (box 1) (box 1))))
  (define same (box 1))
  (check-false (distinct? (list same (box 1) same))))