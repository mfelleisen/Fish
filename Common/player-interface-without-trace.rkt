#lang racket

;; on the Administrative side, the player has contact with
;; -- an tournament administrator
;; -- many referees
;; so there are two interfaces and a combined one for the implementor of the component
;; 
;; the Referee makes promises about how it calls the players during a game
;;
;; a GameObserver can be passed to Referee for/by neutral parties

;; protocol:
;; -- start-of-tournament
;; 
;; -- playing-as is called first and once per game
;; -- playing-with is called at the beginning of a game, after playing-as, once per turn 
;; -- initial is called as (6 - N) times where N is the number of players 
;; -- take-turn is called repeatedly until the game is over
;;
;; -- end-of-tournament is called at the end of the entire tournament
;;
;; The protocol is abondoned when this player raises an exception, diverges, or delivers a bad result.

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

(provide
 (contract-out
  (ready-time        (parameter/c (and/c real? positive?))))

 player%/c  ;; a contract that describes the player class's interface to the administrator 
 player/c   ;; contracts for objects 
 
 SLEEP   ;; for how many seconds is a game state visible via the observer
  
 (contract-out
  (game-observer/c contract?)) 

 MAX-PLAYERS
 MIN-PLAYERS
 
 #; contract
 ;; a contract for a referee function 
 referee/c)

;                                                                                                  
;                                                                                                  
;        ;                                       ;                             ;                   
;        ;                                       ;                                                 
;        ;                                       ;                                                 
;    ;;; ;   ;;;;   ; ;;;    ;;;;   ; ;;;    ;;; ;   ;;;;   ; ;;;     ;;;    ;;;     ;;;;    ;;;;  
;   ;;  ;;  ;    ;  ;;  ;;  ;    ;  ;;   ;  ;;  ;;  ;    ;  ;;   ;   ;   ;     ;    ;    ;  ;    ; 
;   ;    ;  ;;;;;;  ;    ;  ;;;;;;  ;    ;  ;    ;  ;;;;;;  ;    ;  ;          ;    ;;;;;;  ;      
;   ;    ;  ;       ;    ;  ;       ;    ;  ;    ;  ;       ;    ;  ;          ;    ;        ;;;;  
;   ;    ;  ;       ;    ;  ;       ;    ;  ;    ;  ;       ;    ;  ;          ;    ;            ; 
;   ;;  ;;  ;;   ;  ;;  ;;  ;;   ;  ;    ;  ;;  ;;  ;;   ;  ;    ;   ;   ;     ;    ;;   ;  ;    ; 
;    ;;; ;   ;;;;;  ; ;;;    ;;;;;  ;    ;   ;;; ;   ;;;;;  ;    ;    ;;;    ;;;;;   ;;;;;   ;;;;  
;                   ;                                                                              
;                   ;                                                                              
;                   ;                                                                              
;                                                                                                  

(require Fish/Common/game-state)
(require Fish/Common/board)
(require Fish/Common/penguin)

;                                                                          
;                                                                          
;                                                                          
;                             ;                               ;            
;                             ;                               ;            
;     ;;;    ;;;;   ; ;;;   ;;;;;;   ;;;;     ;;;     ;;;   ;;;;;;   ;;;;  
;    ;   ;  ;;  ;;  ;;   ;    ;      ;;  ;   ;   ;   ;   ;    ;     ;    ; 
;   ;       ;    ;  ;    ;    ;      ;           ;  ;         ;     ;      
;   ;       ;    ;  ;    ;    ;      ;       ;;;;;  ;         ;      ;;;;  
;   ;       ;    ;  ;    ;    ;      ;      ;    ;  ;         ;          ; 
;    ;   ;  ;;  ;;  ;    ;    ;      ;      ;   ;;   ;   ;    ;     ;    ; 
;     ;;;    ;;;;   ;    ;     ;;;   ;       ;;; ;    ;;;      ;;;   ;;;;  
;                                                                          
;                                                                          
;                                                                          
;                                                                          

(define MIN-PLAYERS 2)
(define MAX-PLAYERS 4)

(define READY-TIME 30) ;; the number of s guaranteed for first `take-turn` call

(define ready-time (make-parameter READY-TIME))

(define SLEEP 4)

;; ---------------------------------------------------------------------------------------------------
;; contracts ongame observer
;;
;; show the initial state and return a function that
;; 
;; -- consumes the state S0 before placcement,
;; -- the proposed action A and resulting state S1 (if legal),
;; -- OR a string that explains what went wrong
;;
;; If legal, it draws an arrow from S0 to S1 according to A.
;; Otherwise it just places the string into the text field for announcements. 

(define game-observer/c (-> fishes? (-> fishes? (or/c string? (list/c turn? fishes?)) any)))

;; ---------------------------------------------------------------------------------------------------
;; contracts on players

#; {Contrac Contract Contract -> Contract}
(define (make-referee-player%/c this/c state/c actions/c)
  (class/c
   ;; it has to be a strategy but we don't want to depend on strategy.rkt
   (init-field (strategy object?))
   [playing-as
    ;; the referee informs this player of the assigned color 
    (->m penguin-color/c any)]
   [playing-with
    ;; the referee informs this player of the colors of all players (and implicitly their order)
    (->m [listof penguin-color/c] any)]
   (initial
    ;; this player places one an penguin per set-up initial turn 
    (->m fishes? posn/c))
   [take-turn
    ;; this player moves one penguin per turn
    ;; ASSUME this player's color is the first in the player part of state
    ;; the actions represent what other players have done (in order) since the last time `this`
    ;; player took a turn if any; an empty list says "use the state to make your next move"
    ;; ******************************************************************
    ;; This call simultaneously permits a functional and a stateful protocol.
    ;; ******************************************************************
    (->d ([i this/c] [f state/c] [l actions/c]) #:pre (do-not-call-with-skip f) (r move/c))]))

#; {State -> Boolean}
(define (do-not-call-with-skip f)
  (let ([a* (all-possible-actions f)])
    (and (cons? a*) (not (skip? (first a*))))))

(define admin-player%/c
  (class/c
   (start-of-tournament
    ;; the tournament manager informs this player of the nicknames of all paricipants 
    (->m boolean? any))

   [end-of-tournament
    ;; the tournament manager informs this player of the prize winners,
    ;; a subset of those handed to start-of-tournament 
    (->m boolean? any)]))

(define player%/c (and/c (make-referee-player%/c any/c fishes? (listof turn?)) admin-player%/c))
(define player/c  (instanceof/c player%/c))

;; ---------------------------------------------------------------------------------------------------
;; contracts on referees

;; a plain referee contract that checks less 
(define referee/c
  (->i ([f (or/c #false fishes?)])
       ;; the alternative interface was added after the fact
       ;; so that a referee ca be launched with just a list of players
       ;; plus optional arguments for creating a random state from scratch 
       [#:time-out  (t-o positive?)
        #:lop       (lop (f) (if (boolean? f)
                                 (and/c (listof player/c)
                                        (property/c length (between/c MIN-PLAYERS MAX-PLAYERS)))
                                 (λ _ #t)))
        #:size      (r-w (list/c natural? natural?))
        #:fixed     (fix  natural?) 
        #:observers (o (listof game-observer/c))]
       (r (list/c (listof (listof player/c)) (listof player/c)))))

;                                                                          
;                                                                          
;                              ;             ;;;       ;                   
;                                              ;                           
;                                              ;                           
;    ;;;;    ;;;;    ;;;;    ;;;      ;;;      ;     ;;;    ;;;;;;   ;;;;  
;   ;    ;  ;    ;   ;;  ;     ;     ;   ;     ;       ;        ;;  ;    ; 
;   ;       ;;;;;;   ;         ;         ;     ;       ;       ;;   ;;;;;; 
;    ;;;;   ;        ;         ;     ;;;;;     ;       ;      ;;    ;      
;        ;  ;        ;         ;    ;    ;     ;       ;     ;;     ;      
;   ;    ;  ;;   ;   ;         ;    ;   ;;     ;       ;    ;;      ;;   ; 
;    ;;;;    ;;;;;   ;       ;;;;;   ;;; ;      ;;;  ;;;;;  ;;;;;;   ;;;;; 
;                                                                          
;                                                                          
;                                                                          
;                                                                          

(module+ serialize
  (provide action? action->jsexpr jsexpr->action)

  (require SwDev/Lib/pattern-matching)

  (def/mp action (_ row0 col0 row1 col1)
    #' `[[,(? natural? row0) ,(? natural? col0)] [,(? natural? row1) ,(? natural? col1)]])

  (define (action? a)
    (match a
      [(action a b c d) #t]
      [(? string?) #t]
      [_ #f]))

  (define (action->jsexpr s) (if (symbol? s) (~a s) s))
  
  (define (jsexpr->action s)
    (match s
      [(? (and/c string? (λ (x) (not (regexp-match #px"ERROR" x))))) (string->symbol s)]
      [(action row0 col0 row1 col1) s])))
