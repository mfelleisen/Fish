#lang racket

;; testing the trace contract for refereeing, after injecting a bug 

;; For reasons I don't understand yet, this test cannot be incorporated into
;; the referee modules itself. The third _initial_ test seems to diverge then. 

(require Fish/Admin/referee)
(require Fish/Player/player)
(module+ test
  (require rackunit))

(require Fish/Common/player-interface-error-reporting)
(error-printing? #true)

;; -----------------------------------------------------------------------------
(define player1 (new imperative-player%))
(define player0 (new player%))
(define player2 (new player%))
(define player3 (new player%))
(define player4 (new player%))

(define bad-turn-choice  (new bad-turn-choice%))

(define players-1-2-3 (list player0 player2 player3))

;; -----------------------------------------------------------------------------
(module+ test
  
  (enbug)
  
  (check-exn #px"violated: the 1st clause of trace/c"   
             (λ ()
               (referee #false
                        #:lop (list player1 player2 player3 player4)
                        #:time-out +inf.0
                        #:size (list 4 5))
               (displayln "exit 1"))
             
             "basic checK: the trace contract is violated")
  
  (check-exn #px"violated: the 1st clause of trace/c"
             (λ ()
               (referee #false
                        #:lop (list player1 player2 player3 bad-turn-choice)
                        #:time-out +inf.0
                        #:size (list 4 5))
               (displayln "exit 2"))
             "a player makes a bad move, then the referee goes out of order")

  (debug))
