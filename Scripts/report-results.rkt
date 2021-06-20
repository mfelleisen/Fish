#lang racket

(provide
 #; {[Listof String] [Listof [Listof [Instanceof Player]]] [Listof [Instanceof Player]] -> Void}
 report-results)

;; ---------------------------------------------------------------------------------------------------
(require Fish/Player/human)

;; ---------------------------------------------------------------------------------------------------
(define (report-results names players rankings failures)
  (for ([f failures]) (displayln `[,(match-name names players f) failed or cheated]))
  (for ([r rankings] [p (in-naturals)])
    (for ([a-player r])
      (displayln `[,(match-name names players a-player) placed ,(place (+ p 1))]))))

#; {[Listof String] [Listof Player] Player -> String}
#; {ASSUME (= (length names) (length players))}
(define (match-name names players f)
  (if (is-a? f human%)
      "the Human"
      (for/first ([p players][n names] #:when (equal? f p)) n)))

#; {N -> String}
(define (place p)
  (case p
    [(1) "first"]
    [(2) "second"]
    [(3) "third"]
    [(4) "last"]
    [else (~a p "th")]))