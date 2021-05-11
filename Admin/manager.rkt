#lang racket

;; a tournament manager that plays a complete tournament with players given ranked by "age"
;; and produces the list of first-placed players; it informs all non-cheaters whether they were
;; first-placed or not (boolean)

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

(require (only-in Fish/Common/player-interface player/c))
(require Fish/Lib/list)

(define player*/c [listof player/c])
(define results/c (list/c player*/c player*/c))

(provide
 (contract-out
  (results/c contract?)
  [manager
   ;; produces a list consisting of the tournament winners and failures/cheaters 
   (->i ([lop (and/c player*/c cons? distinct?)]) (#:fixed [f natural?] #:time-out (t-o positive?))
        (r results/c))]))

(module+ examples
  (provide
   six six-expected
   10mixed 10mixed-counted

   2players 2players-counted
   5players 5players-counted
   10players 10players-counted
   6players 6players-counted
   2+bads 2+bads-counted
   6+bads 6+bads-counted))

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

(require (except-in Fish/Common/player-interface player/c))
(require Fish/Admin/prepare-games)
(require Fish/Admin/referee)
(require Fish/Lib/xsend)

(module+ examples
  (require (submod Fish/Admin/referee examples)))

(module+ test
  (require (submod ".." examples)))

(module+ test
  (require (submod ".."))
  (require (submod Fish/Admin/referee examples))
  (require rackunit))

;                                                   
;                                                   
;                                                   
;                                                   
;  ;;;;;;  ;;;;   ; ;;   ;;;;    ;;;;   ;;;    ;;;; 
;  ;  ;  ;     ;  ;;  ;      ;  ;;  ;  ;;  ;   ;;  ;
;  ;  ;  ;     ;  ;   ;      ;  ;   ;  ;   ;;  ;    
;  ;  ;  ;  ;;;;  ;   ;   ;;;;  ;   ;  ;;;;;;  ;    
;  ;  ;  ; ;   ;  ;   ;  ;   ;  ;   ;  ;       ;    
;  ;  ;  ; ;   ;  ;   ;  ;   ;  ;; ;;  ;       ;    
;  ;  ;  ;  ;;;;  ;   ;   ;;;;   ;;;;   ;;;;   ;    
;                                   ;               
;                                ;  ;               
;                                 ;;                

#; {type Player* = [Listof Player]}
#; {type Results = [List Player* Player*]}
#; {type Referee = [[Listof Player] -> [List [Listof [Listof Player]] [Listof Player]]]}
#; {type Action  = }

#; {Player* -> Results}
;; produce a pair of the wunners and the cheaters 
(define (manager lop0 #:time-out (t-o 3) #:fixed [f #f] #:size (s (list 5 5)))
  (define run-one-game
    (if f
        (λ (lop) (referee #f #:size s #:time-out t-o #:lop lop #:fixed f))
        (λ (lop) (referee #f #:size s #:time-out t-o #:lop lop))))

  (match-define (list starters cheaters0)
    (inform-all/not-cheaters (λ (p msg) (xsend p start-of-tournament msg)) lop0 '[] '[]))
  
  (define-values (ranks cheaters) (run-all-games starters run-one-game))
  (define winners (first ranks))
  (define cheaters1   (append cheaters0 cheaters))
  (define live-losers (filter (λ (p) (not (or (member p winners) (member p cheaters1)))) lop0))
  
  (inform-all/not-cheaters (λ (p msg) (xsend p end-of-tournament msg)) winners live-losers cheaters1))

;; ---------------------------------------------------------------------------------------------------
#;{[Listof Player] Referee -> (values [Listof Player] [Listof Player])}
;; generative recursion: terminates because either the number of players shrinks per round
;; or the tournament is forcibly stopped because the surviving winners all tie for first place
(define (run-all-games lop0 run-one-game)
  ;; accumulators: previous-winners and cheats 
  (let loop ([lop1 lop0] [previous-winners '()] [cheats '()])
    (define lop  (re-sort lop1 lop0))
    (define lop# (length lop))
    (cond
      ;; not enough for one game 
      [(< lop# MIN-PLAYERS) 
       (values (list lop) cheats)]
      ;; just enough for one game 
      [(<= lop# MAX-PLAYERS)
       (match-define [list ranked new-cheats] (run-one-game lop))
       (values ranked (append new-cheats cheats))]
      [else ;; keep going with rounds of games 
       (match-define `[,winners ,new-cheats] (run-one-round-of-games lop run-one-game))
       (if (equal? winners previous-winners)
           (values winners (append new-cheats cheats))
           (loop winners lop# (append new-cheats cheats)))])))

;; ---------------------------------------------------------------------------------------------------
#; {Player* Referee -> [List Player* Player*]}
(define (run-one-round-of-games lop run-one-game)
  (define games    (prepare-games MIN-PLAYERS MAX-PLAYERS lop))
  (define results  (map run-one-game games))
  (define winners
    (append-map first
                (filter-map 
                 (λ (r)
                   (match-define [list ranked _] r)
                   (match ranked
                     ['[] #f]
                     [_ ranked]))
                 results)))
  (define cheaters (append-map second results))
  (list winners cheaters))

;; ---------------------------------------------------------------------------------------------------
#;{Player* Player* -> Player*}
;; sort list of winners according to lop0 
(define (re-sort winners lop0)
  (filter (λ (x) (member x winners)) lop0))

(module+ test
  (define box% (class object% [init content] (super-new)))
  (match-define (list box1 box2 box3 box4) (map (λ (x) (new box% [content x])) '(1 2 3 4)))
  (check-equal? (re-sort (list box2 box1 box3) (list box1 box2 box3 box4)) (list box1 box2 box3)))

;                                                                        
;                    ;;                                                  
;      ;            ;                                       ;;;    ;;;   
;                   ;                                         ;      ;   
;    ;;;   ; ;;   ;;;;;   ;;;    ;;;; ;;;;;;         ;;;;     ;      ;   
;      ;   ;;  ;    ;    ;; ;;   ;;  ;;  ;  ;            ;    ;      ;   
;      ;   ;   ;    ;    ;   ;   ;    ;  ;  ;            ;    ;      ;   
;      ;   ;   ;    ;    ;   ;   ;    ;  ;  ;         ;;;;    ;      ;   
;      ;   ;   ;    ;    ;   ;   ;    ;  ;  ;        ;   ;    ;      ;   
;      ;   ;   ;    ;    ;; ;;   ;    ;  ;  ;        ;   ;    ;      ;   
;    ;;;;; ;   ;    ;     ;;;    ;    ;  ;  ;         ;;;;     ;;     ;; 
;                                                                        
;                                                                        
;                                                                        
  
#; {Action [Listof Player] [Listof Player] [Listof Player] -> Results}
;; EFFECT inform winners and losers; move players that fail this message into cheaters 
(define (inform-all/not-cheaters action winners0 losers0 cheaters0)
  (define-values (winners cheats1)
    (for/fold ([winners '()][cheats cheaters0]) ([p (in-list winners0)])
      (inform-one action p #true winners cheats)))
  ;; --- if `losers0` is empty, `losers` is empty
  (define-values (losers cheaters)
    (for/fold ([losers '()][cheats2 cheats1]) ([r losers0])
      (inform-one action r #false losers cheats2)))
  ;; --- 
  (list (reverse winners) (reverse cheaters)))

#; {Action [Player Boole [Listof Player] [Listof Player] -> (values [Listof Player] [Listof Player])]}
(define (inform-one action p msg winners cheats)
  (define void-failed (action p msg))
  (if (failed? void-failed)
      (values winners (cons p cheats))
      (values (cons p winners) cheats)))

;                                     
;                                     
;     ;                    ;          
;     ;                    ;          
;   ;;;;;   ;;;    ;;;   ;;;;;   ;;;  
;     ;    ;;  ;  ;   ;    ;    ;   ; 
;     ;    ;   ;; ;        ;    ;     
;     ;    ;;;;;;  ;;;     ;     ;;;  
;     ;    ;          ;    ;        ; 
;     ;    ;      ;   ;    ;    ;   ; 
;     ;;;   ;;;;   ;;;     ;;;   ;;;  
;                                     
;                                     
;                                     

(module+ examples
  (define 2players (many-players 2))
  (define 2players-counted '[1 0])

  (define 5players (many-players 5))
  (define 5players-counted '[1 0])

  (define 6players (many-players 6))
  (define 6players-counted '[1 0])

  (define 10players (many-players 10))
  (define 10players-counted '[1 0])
  
  (define bads (list bad-playing-as bad-playing-with bad-turn-choice bad-end-of-tournament))
  (define 6+bads (append (many-players 6) bads))
  (define 6+bads-counted `[[1 ,(length bads)] [2 ,(- (length bads) 1)]])

  (define all-bad (cons bad-start-of-tournament bads))
  (define 2+bads (append #; 2players all-bad))
  (define 2+bads-counted `[[0 ,(length all-bad)] [0 ,(- (length all-bad) 1)]])
  (define 10mixed (append (many-players (- 10 (length all-bad))) all-bad))
  (define 10mixed-counted `[[1 ,(length all-bad)] [2 ,(- (length all-bad) 1)]])

  (define six (append players-1-2-3 (list bad-playing-as bad-playing-with bad-turn-choice)))
  (define six-expected players-1-2-3))

(module+ test

  (define one (many-players 1))
  (check-equal? (manager one #:fixed 2) (list one '[]) "cover the `less than min-player' case")
  
  (check-true (cons? (manager (many-players 2))) "cover the random fish on tiles case")
    
  (check-equal? (first (manager players-1-2-3 #:fixed 2)) (list (first players-1-2-3)))

  (check-exn #px"distinct" (λ () (manager (append players-1-2-3  players-1-2-3) #:fixed 2)))

  (check-equal? (first (manager six #:fixed 2)) (list (first players-1-2-3)))

  (define (count in)
    (match-define [list winners cheats-and-failures] (manager in #:fixed 2))
    (list (length winners) (length cheats-and-failures)))

  (check-equal? (count 2players) 2players-counted)
  (check-equal? (count 5players) 5players-counted)
  (check-equal? (count 6players) 6players-counted)
  (check-equal? (count 10players) 10players-counted)
  (check-equal? (count 10mixed) (first 10mixed-counted))
  (check-equal? (count 2+bads)  (first 2+bads-counted) "the 1 winner fails")
  
  (check-equal? (count 6+bads) (first 6+bads-counted)))
