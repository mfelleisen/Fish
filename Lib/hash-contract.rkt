#lang racket

(provide
 (contract-out
  [hash-carrier/c (-> list? contract?)]))

;; -----------------------------------------------------------------------------
(define (hash-carrier/c domain [range any/c])
  (and/c
   (hash/c any/c range)
   (λ (h)
     (for/and ([d domain])
       (hash-ref h d #false)))))

;; -----------------------------------------------------------------------------
(module+ test
  (require rackunit)

  (define server-options '[a b c])
  (define/contract (f h)
    (-> (hash-carrier/c server-options) 0)
    0)

  (check-equal? (f (hash 'a 0 'b 1 'c 1)) 0 "exact")
  (check-equal? (f (hash 'a 0 'b 1 'c 1 'd 2)) 0 "too much")
  (check-exn exn:fail:contract? (λ () (f (hash 'a 0 'b 1 'd 1))) "too little")

  (define/contract (g h)
    (-> (hash-carrier/c server-options boolean?) 0)
    0)
  (check-exn exn:fail:contract? (λ () (g (hash 'a 0 'b 1 'c 1 'd 2))) "range"))
  
  
    