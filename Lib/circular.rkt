#lang racket

(provide create-circular-list-stream)

;; ---------------------------------------------------------------------------------------------------
(define (create-circular-list-stream v)
  (circular-list-stream v 0 (length v)))

(struct circular-list-stream (v i l)
  #:methods gen:stream
  [(define (stream-empty? stream) #false)
   (define (stream-first stream)
     (list-ref (circular-list-stream-v stream) (circular-list-stream-i stream)))
   (define (stream-rest stream)
     (struct-copy
      circular-list-stream stream
      [i (modulo (+ (circular-list-stream-i stream) 1) (circular-list-stream-l stream))]))])

;; ---------------------------------------------------------------------------------------------------
(module+ test 

  (define s123 (create-circular-list-stream '[1 2 3]))
  (for/list ([x (in-stream s123)][i (in-range 12)]) x))