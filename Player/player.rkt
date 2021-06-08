#lang racket

;; this component implements the mechanics of the player

;                                                                  
;                                                                  
;                                                            ;;;   
;                     ;                                        ;   
;                     ;                                        ;   
;    ;;;;   ;;  ;;  ;;;;;;   ;;;;    ;;;;   ; ;;;     ;;;      ;   
;   ;    ;   ;  ;     ;     ;    ;   ;;  ;  ;;   ;   ;   ;     ;   
;   ;;;;;;    ;;      ;     ;;;;;;   ;      ;    ;       ;     ;   
;   ;         ;;      ;     ;        ;      ;    ;   ;;;;;     ;   
;   ;         ;;      ;     ;        ;      ;    ;  ;    ;     ;   
;   ;;   ;   ;  ;     ;     ;;   ;   ;      ;    ;  ;   ;;     ;   
;    ;;;;;  ;    ;     ;;;   ;;;;;   ;      ;    ;   ;;; ;      ;;;
;                                                                  
;                                                                  
;                                                                  
;                                                                  

(require Fish/Common/player-interface)

(define MIN-DEPTH 1)
(define MAX-DEPTH 5)
(define (good-depth d)
  (and (integer? d) (<= MIN-DEPTH d MAX-DEPTH)))

(provide
 MIN-DEPTH
 MAX-DEPTH 
  
 (contract-out
  (good-depth contract?)
  
  [player%                  player%/c] ;; a functional player that uses the fixed strategy
  (imperative-player%       player%/c)

  [bad-start-of-tournament% player%/c] ;; raises exn 
  [bad-playing-as%          player%/c] ;; raises exn 
  [bad-playing-with%        player%/c] ;; raises exn 
  [bad-init-time%           player%/c] ;; times out for initial placement request 
  [bad-init-choice%         player%/c] ;; pick any position that's taken (or #false)
  [bad-turn-time%           player%/c] ;; choose an illegal turn action
  (bad-turn-choice%         player%/c) ;; takes the first penguin to the second 
  [bad-end-of-tournament%   player%/c])); raises exn 

;                                                                                      
;       ;                                  ;                                           
;       ;                                  ;                          ;                
;       ;                                  ;                                           
;    ;;;;   ;;;   ;;;;    ;;;   ; ;;    ;;;;   ;;;   ; ;;    ;;;    ;;;    ;;;    ;;;  
;   ;; ;;  ;;  ;  ;; ;;  ;;  ;  ;;  ;  ;; ;;  ;;  ;  ;;  ;  ;;  ;     ;   ;;  ;  ;   ; 
;   ;   ;  ;   ;; ;   ;  ;   ;; ;   ;  ;   ;  ;   ;; ;   ;  ;         ;   ;   ;; ;     
;   ;   ;  ;;;;;; ;   ;  ;;;;;; ;   ;  ;   ;  ;;;;;; ;   ;  ;         ;   ;;;;;;  ;;;  
;   ;   ;  ;      ;   ;  ;      ;   ;  ;   ;  ;      ;   ;  ;         ;   ;          ; 
;   ;; ;;  ;      ;; ;;  ;      ;   ;  ;; ;;  ;      ;   ;  ;;        ;   ;      ;   ; 
;    ;;;;   ;;;;  ;;;;    ;;;;  ;   ;   ;;;;   ;;;;  ;   ;   ;;;;   ;;;;;  ;;;;   ;;;  
;                 ;                                                                    
;                 ;                                                                    
;                 ;                                                                    

(require Fish/Player/strategy-interface)
(require (except-in Fish/Common/game-tree))
(require Fish/Common/game-state)
(require Fish/Common/internal-player)
(require Fish/Common/penguin)

(module+ test
  (require (submod ".."))
  (require Fish/Player/greedy)
  (require (submod Fish/Player/greedy examples))
  (require rackunit))

(module+ test-time
  (require (submod ".."))
  (require (submod Fish/Player/greedy examples))
  (require Fish/Common/internal-player)
  (require Fish/Lib/xsend)
  (require rackunit))

;                              
;   ;                          
;   ;                          
;   ;                          
;   ;;;;   ;;;;    ;;;    ;;;  
;   ;; ;;      ;  ;   ;  ;;  ; 
;   ;   ;      ;  ;      ;   ;;
;   ;   ;   ;;;;   ;;;   ;;;;;;
;   ;   ;  ;   ;      ;  ;     
;   ;; ;;  ;   ;  ;   ;  ;     
;   ;;;;    ;;;;   ;;;    ;;;; 
;                              
;                              
;

;; internals of Player
;; internally, the player is game mechanics while the strategy component makes game decisions 

(define base-player%
  (class object% (init-field strategy)
    (field (me  (first penguin-colors)))
    (field (other-players '()))
    (field (tree #false))

    [define/public (playing-as my-name)
      (set! me my-name)]
    
    [define/public (playing-with others)
      (set! other-players others)]
    
    (define/public (initial state)
      ((strategy-place strategy) state))
    
    [define/public (take-turn state actions-since-last-turn)
      (error 'take-turn "abstract")]

    (define/public (start-of-tournament nicknames)
      (void))
    
    [define/public (end-of-tournament results)
      (void)]
    
    (super-new)))

(define player%
  (class base-player%
    (inherit-field strategy me other-players tree)
    
    [define/override (take-turn state actions-since-last-turn)
      (set! tree (generate-tree state))
      ;; I could update the tree here but I'll just stay functional
      ((strategy-move strategy) tree)]
    
    (super-new)))

(define imperative-player%
  (class base-player%
    (inherit-field strategy me other-players tree)
    
    [define/override (take-turn state actions-of-others-since-last-turn)
      (set! tree 
            (if (empty? actions-of-others-since-last-turn)
                (generate-tree state)
                (apply tree-path tree actions-of-others-since-last-turn)))
      (define best-action ((strategy-move strategy) tree))
      (set! tree (tree-path tree best-action))
      best-action]
    
    (super-new)))

(module+ test
  (define player (new player% [strategy greedy-strategy]))

  (check-equal? (send player playing-as (second penguin-colors)) (void))
  (check-equal? (send player playing-with (list (third penguin-colors))) (void))
  (check-equal? (send player initial 2-state-no-action-2) B2-state-no-action-2)
  
  (check-exn exn:fail:contract? (λ () (send player take-turn 2-state-no-action-2 '[])))
  (check-equal? (send player take-turn 2-state-1-action-7 '[]) A2-state-1-action-7)
  (check-exn exn:fail:contract? (λ () (send player take-turn 2-state-0-action-1player2 '[])))
  (check-equal? (send player take-turn 2-state-2 '[]) A2-state-2)
  (check-equal? (send player take-turn 3-state-2 '[]) A3-state-2)

  (check-equal? (send player start-of-tournament #t) (void))
  (check-equal? (send player end-of-tournament #t) (void)))

(module+ test
  (define player! (new imperative-player% [strategy greedy-strategy]))

  (check-equal? (send player! playing-as (second penguin-colors)) (void))
  (check-equal? (send player! playing-with (list (third penguin-colors))) (void))
  (check-equal? (send player! initial 2-state-no-action-2) B2-state-no-action-2)

  (check-exn exn:fail:contract? (λ () (send player! take-turn 2-state-no-action-2 '[])))
  (check-equal? (send player! take-turn 2-state-1-action-7 '[]) A2-state-1-action-7)
  (check-exn exn:fail:contract? (λ () (send player! take-turn 2-state-0-action-1player2 '[])))
  (check-equal? (send player! take-turn 2-state-2 '[]) A2-state-2)
  (check-equal? (send player! take-turn 3-state-2 '[]) A3-state-2)

  (check-equal? (send player! start-of-tournament #t) (void))
  (check-equal? (send player! end-of-tournament #t) (void)))

;                              
;   ;                 ;        
;   ;                 ;        
;   ;                 ;        
;   ;;;;   ;;;;    ;;;;   ;;;  
;   ;; ;;      ;  ;; ;;  ;   ; 
;   ;   ;      ;  ;   ;  ;     
;   ;   ;   ;;;;  ;   ;   ;;;  
;   ;   ;  ;   ;  ;   ;      ; 
;   ;; ;;  ;   ;  ;; ;;  ;   ; 
;   ;;;;    ;;;;   ;;;;   ;;;  
;                              
;                              
;                              

;; players that misbehave w/ something that could be an action but isn't:

(define-syntax-rule (define/override-m % m) (define % (class player% (super-new) m)))

(define/override-m bad-init-choice%  ;; break contract or pick any position that's taken 
  (define/override (initial state)
    (define players (fishes-players state))
    (for*/first ([p players] [penguins (in-value (iplayer-places p))] #:when (cons? penguins))
      (first penguins))))

(define/override-m bad-turn-choice% ;; chooses an action that takes the first penguin to itself 
  (define/override (take-turn state actions)
    (define players (fishes-players state))
    (define my-penguins (iplayer-places (first players)))
    (list (first my-penguins) (first my-penguins))))

;; players that go into infinite loops:

(define-syntax-rule (define/time % m) (define/override-m % (define/override (m . x) (let L () (L)))))

(define/time bad-init-time% initial)

(define/time bad-turn-time% take-turn)

;; players that raise exceptions 
(define-syntax-rule (define/raise % m n) (define/override-m % (define/override (m . x) (raise n))))

(define/raise bad-playing-as% playing-as 0)

(define/raise bad-playing-with% playing-with 1)

(define/raise bad-end-of-tournament% end-of-tournament 3)

(define/raise bad-start-of-tournament% end-of-tournament 4)

(module+ test-time
  (define bad-playing-as [new bad-playing-as%    ]) ;; raises exn
  (check-exn (=/c 0) (λ () (send bad-playing-as playing-as (first penguin-colors))) "1")

  (define bad-playing-with [new bad-playing-with%  ]) ;; raises exn
  (check-equal? (send bad-playing-with playing-as (first penguin-colors)) (void))
  (check-exn (=/c 1) (λ () (send bad-playing-with playing-with (rest penguin-colors))) "2")

  (define bad-init-time [new bad-init-time%     ]) ;; times out for initial placement request
  (check-equal? (send bad-init-time playing-as (first penguin-colors)) (void))
  (check-equal? (send bad-init-time playing-with (rest penguin-colors)) (void))
  (check-true (failed? (xsend bad-init-time initial 2-state-no-action-2)) "3")

  (define bad-init-choice [new bad-init-choice%   ]) ;; pick any position that's taken (new or #false)
  (check-equal? (send bad-init-choice playing-as (first penguin-colors)) (void))
  (check-equal? (send bad-init-choice playing-with (rest penguin-colors)) (void))
  (check-exn exn:fail:contract? (λ () (send bad-init-choice initial 2-state-no-action-2)) "4")

  (define bad-turn-time [new bad-turn-time%     ]) ;; choose an illegal turn action
  (check-true (failed? (xsend bad-turn-time take-turn 2-state-1-action-7)) "5")
  
  (define bad-turn-choice (new bad-turn-choice%   )) ;; takes the first penguin to the second
  (define 2-state-1-action-7-penguins (iplayer-places (first (fishes-players 2-state-1-action-7))))
  (check-equal? (send bad-turn-choice take-turn 2-state-1-action-7 '[])
                (list (first 2-state-1-action-7-penguins) (first 2-state-1-action-7-penguins)))

  (define bad-end-of-tournament [new bad-end-of-tournament%])
  (check-exn (=/c 3) (λ () (send bad-end-of-tournament end-of-tournament #true)) "6"))
