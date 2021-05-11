#lang racket

;; a referee that plays a single game with players given in the order of their "age"
;; and produces a ranking (list of players placed at same position) and a list of cheats

;; ---------------------------------------------------------------------------------------------------
;; This "bug switch" exists only for the trace-contract test. See trace-test.
;; In the second rouund, the bug will send a take-turn message to a third player out of order. 
(provide enbug debug)

(define no-bug? #true)
(define (enbug) (set! no-bug? #false))
(define (debug) (set! no-bug? #true))

;; ---------------------------------------------------------------------------------------------------

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

(require (only-in Fish/Common/player-interface referee/c))

(provide
 (contract-out
  ;; run a complete game from initial moves thru final stages for the given external players
  ;; the list is sorted in ascending order of age; produce list of ranked players and cheaters
  ;; EFFECT provide observers with updates on regular turns
  (referee referee/c)))

(module+ examples
  (provide
   ;; individual players 
   player1 player2 player3 player4 
   bad-playing-as
   bad-playing-with
   bad-turn-choice
   bad-turn-time
   bad-start-of-tournament
   bad-end-of-tournament

   ;; groups of players 
   players-1-2-3
   iplayers-1-2-3 
   one-good-one-bad
   all-imperative

   ;; N -> [Listof Player]
   many-players 

   ;; game states: 
   state-2-9-with-three-regular-players
   istate-2-9-with-three-regular-players))

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

(require (except-in Fish/Common/player-interface referee/c))
(require Fish/Admin/game-observer)
(require Fish/Common/internal-player)
(require Fish/Common/game-tree)
(require Fish/Common/game-state)
(require Fish/Lib/list)
(require Fish/Lib/xsend)

(module+ examples
  (require Fish/Player/player))

(module+ test 
  (require (submod ".." examples))
  (require (submod ".."))
  (require Fish/Player/player)
  (require rackunit)
  (require SwDev/Testing/check-values))

(module+ picts
  (require (submod ".." examples))
  (require Fish/Player/player))

(module+ profile
  (require (submod ".." examples))
  (require profile))

(module+ time
  (require (submod ".." examples))
  (require Fish/Player/player))


;                                                                 
;       ;                                                         
;       ;           ;                                             
;       ;           ;                                             
;    ;;;;  ;;;;   ;;;;;  ;;;;           ;;;;   ;;;   ;;;;         
;   ;; ;;      ;    ;        ;          ;;  ; ;;  ;  ;; ;;        
;   ;   ;      ;    ;        ;          ;     ;   ;; ;   ;        
;   ;   ;   ;;;;    ;     ;;;;          ;     ;;;;;; ;   ;        
;   ;   ;  ;   ;    ;    ;   ;          ;     ;      ;   ;        
;   ;; ;;  ;   ;    ;    ;   ;          ;     ;      ;; ;;   ;;   
;    ;;;;   ;;;;    ;;;   ;;;;          ;      ;;;;  ;;;;    ;;   
;                                                    ;            
;                                                    ;            
;                                                    ;            

#; {type Tiles* = [Listof TileIndex]}

#; {type Rankings = [Listof Player*]}
#; (type Player*  = [Listof Player])
#; {type Observer* = [Listof Observer]}

(define DEFAULT-ROWS 2)
(define DEFAULT-COLUMNS 9)

(module+ examples ;; create some players
  (define player1 (new player%))
  (define iplayer1 (new imperative-player%))
  (define player2 (new player%))
  (define iplayer2 (new imperative-player%))
  (define player3 (new player%))
  (define iplayer3 (new imperative-player%))
  (define player4 (new player%))
  (define players-1-2-3 (list player1 player2 player3))
  (define iplayers-1-2-3 (list iplayer1 player2 player3))
  (define all-imperative (list iplayer1 iplayer2 iplayer3))

  (define (many-players n) (for/list ([_ (in-range n)]) (new player% [depth 2])))

  (define bad-start-of-tournament (new bad-start-of-tournament%))
  (define bad-playing-as   (new bad-playing-as%))
  (define bad-playing-with (new bad-playing-with%))
  (define bad-turn-choice  (new bad-turn-choice%))
  (define bad-turn-time    (new bad-turn-time%))
  (define bad-end-of-tournament (new bad-end-of-tournament%))

  (define one-good-one-bad (list bad-turn-choice player1))

  (define state-2-9-with-three-regular-players (create-state 2 9 players-1-2-3 #:fixed 2))
  (define istate-2-9-with-three-regular-players (create-state 2 9 iplayers-1-2-3 #:fixed 2)))

;                                                   
;                    ;;                             
;                   ;                               
;                   ;                               
;    ;;;;   ;;;   ;;;;;   ;;;    ;;;;   ;;;    ;;;  
;    ;;  ; ;;  ;    ;    ;;  ;   ;;  ; ;;  ;  ;;  ; 
;    ;     ;   ;;   ;    ;   ;;  ;     ;   ;; ;   ;;
;    ;     ;;;;;;   ;    ;;;;;;  ;     ;;;;;; ;;;;;;
;    ;     ;        ;    ;       ;     ;      ;     
;    ;     ;        ;    ;       ;     ;      ;     
;    ;      ;;;;    ;     ;;;;   ;      ;;;;   ;;;; 
;                                                   
;                                                   
;                                                   

(define (referee pre-state
                 #:time-out (time-out (time-out-limit))
                 #:observers (o*0 '())
                 #:lop   (lop '[])
                 #:size  (r-w #false )
                 #:fixed (fix #false))
  
  (time-out-limit time-out)
  
  (define state0 (if (cons? lop) (prepare-state lop r-w fix) pre-state))

  (define external* (map iplayer-payload (fishes-players state0)))
  
  (match-define (list post-inform cheaters0) (inform-about-self-and-others state0 external*))
  (define external*1 (remove* cheaters0 external*))
  
  (match-define (list post-placement cheaters1) (initial-placements post-inform external*1))
  (define external*2 (remove* cheaters1 external*1))
  
  (define o* (map (λ (uninit-observer) (uninit-observer post-placement)) o*0))
  
  (set! first-round #true)
  (match-define (list post-game cheaters2) (play-game (generate-tree post-placement) external*2 o*))
  (define ranked (rank-players (fishes-players post-game) external*2))
  
  (list ranked (append cheaters2 cheaters1 cheaters0)))

;; ---------------------------------------------------------------------------------------------------
#; {[NEListof Player] [Maybe [List N N]] [Maybe N] -> State}
(define (prepare-state lop r-w fix)
  (match-define (list row column) (or r-w (list DEFAULT-ROWS DEFAULT-COLUMNS)))
  (if fix
      (create-state row column lop #:fixed fix)
      (create-state row column lop)))

;; ---------------------------------------------------------------------------------------------------
#; {Player* Player* -> Rankings}
(define (rank-players players player*)
  (define sorted  (sort players > #:key iplayer-score))
  (define ranking (group-by iplayer-score sorted =))
  (map (λ (group) (map iplayer-payload group)) ranking))

(module+ test
  
  (check-equal? (caar (referee state-2-9-with-three-regular-players)) players-1-2-3 "3 good ones")
  (check-equal? (caar (referee istate-2-9-with-three-regular-players)) iplayers-1-2-3 "3 i-good ones")

  (check-equal? (caar (referee #f #:lop players-1-2-3 #:size '[2 9] #:fixed 2)) players-1-2-3 "3 too")
  (check-true (cons? (referee #f #:lop players-1-2-3)) "3 too")
 
  (define b49 (create-state 4 9 (list bad-turn-choice bad-turn-time bad-playing-with bad-playing-as)))
  (check-equal? (length (second (referee b49))) 4 "four bad ones"))

;                                            
;                    ;;                      
;      ;            ;                        
;                   ;                        
;    ;;;   ; ;;   ;;;;;   ;;;    ;;;; ;;;;;; 
;      ;   ;;  ;    ;    ;; ;;   ;;  ;;  ;  ;
;      ;   ;   ;    ;    ;   ;   ;    ;  ;  ;
;      ;   ;   ;    ;    ;   ;   ;    ;  ;  ;
;      ;   ;   ;    ;    ;   ;   ;    ;  ;  ;
;      ;   ;   ;    ;    ;; ;;   ;    ;  ;  ;
;    ;;;;; ;   ;    ;     ;;;    ;    ;  ;  ;
;                                            
;                                            
;                                            

#; {State Player* -> (list State Player*)}
;; EFFECT tell each player about itself (color) and the other players (colors);
;; return state that represents surviving players 
(define (inform-about-self-and-others state0 __external*)
  (define players (fishes-players state0))
  (define all-avatars (map iplayer-color players))

  (for/fold ([state state0] [cheats '()] #:result (list state (reverse cheats))) ([ip players])
    (define avatar (iplayer-color ip))
    (define external (iplayer-payload ip))
    (define void-failed (xsend external playing-as  #:caller "referee.rkt" avatar))
    (cond
      [(failed? void-failed)
       (values (delete-player state avatar) (cons external cheats))]
      [else
       (define void-failed
         (xsend external playing-with #:caller "referee.rkt" (remove avatar all-avatars)))
       (cond
         [(failed? void-failed)
          (values (delete-player state avatar) (cons external cheats))]
         [else
          (values state cheats)])])))

(module+ test  
  (define players-1-bad-as-bad-with (list player1 bad-playing-as bad-playing-with))

  (let-values ([(state0) (create-state 8 8 (list player1 player2 player3))])
    (check-equal? (second (inform-about-self-and-others state0 '()))
                  '[]
                  "all good"))
  (let-values ([(state0) (create-state 8 8 players-1-bad-as-bad-with)])
    (check-equal? (second (inform-about-self-and-others state0 '()))
                  (rest players-1-bad-as-bad-with)
                  "recognize bad players")))

;                                                   
;                                                   
;      ;             ;     ;       ;          ;;;   
;                          ;                    ;   
;    ;;;   ; ;;    ;;;   ;;;;;   ;;;   ;;;;     ;   
;      ;   ;;  ;     ;     ;       ;       ;    ;   
;      ;   ;   ;     ;     ;       ;       ;    ;   
;      ;   ;   ;     ;     ;       ;    ;;;;    ;   
;      ;   ;   ;     ;     ;       ;   ;   ;    ;   
;      ;   ;   ;     ;     ;       ;   ;   ;    ;   
;    ;;;;; ;   ;   ;;;;;   ;;;   ;;;;;  ;;;;     ;; 
;                                                   
;                                                   
;                                                   

#; {State Player* -> (list State Player*)}
;; every player gets to place one penguin at a time, until all players have placed all their penguins
(define (initial-placements state0 player*)
  
  (define penguin# (assigned-penguins player*))
  ;; play the entire init phase:
  (for/fold ([o-state state0] [o-cheats '()] #:result `(,o-state ,o-cheats)) ([_ penguin#])
    ;; play one init round:
    (define still-active (remove* o-cheats player*))
    (for/fold ([i-state o-state] [i-cheats o-cheats]) ([player still-active])
      (define-values (state cheated?) (one-initial-turn i-state))
      (values state (if cheated? (cons player i-cheats) i-cheats)))))

(module+ test 
    
  (define state0 (create-state 3 3 players-1-2-3 #:fixed 2))
  (define state1 (next-player (place-avatar state0 '[0 0])))
  (define state2 (create-state 3 3 '(x y) #:holes '[ [0 0] ]))

  (define players*1 (reverse (rest (reverse players-1-2-3)))) ;; all but last 
  (check-equal? (let* ([state (first (initial-placements state0 players*1))])
                  (set-count (apply set (append-map iplayer-places (fishes-players state)))))
                (* (length players*1) (- PENGUIN-N (length players*1)))
                "good players grab yeah-many unique places")
  (check-equal? (second (initial-placements state0 players*1)) '[] "good players don't cheat")
  
  (define players*2 (list player1 (new bad-init-time%)))
  (define bst0 (create-state 3 3 players*2 #:fixed 2))
  (check-equal? (second (initial-placements bst0 players*2)) (rest players*2) "good player survives")
  
  (check-equal? (let* ([state (first (initial-placements bst0 players*2))])
                  (set-count (apply set (append-map iplayer-places (fishes-players state)))))
                (* (- (length players*2) 1) (- PENGUIN-N (length players*2)))
                "one bad player gets kicked, but the good ones play"))
      
;; ---------------------------------------------------------------------------------------------------
#; {State -> (values State Boolean)}
;; ask `i` to pick the next posn for a penguin to produce a new state, with player removed if cheating
;; EFFECT MASK a failure to respond or an illegal placement gets `i` moved to `cheats`
;; ASSUME color of `i` is color of first player in `state`
(define (one-initial-turn state)

  (define active (first (fishes-players state)))
  (define avatar (iplayer-color active))
  (define external (iplayer-payload active))
  (define choice-failed (xsend external initial #:caller "referee.rkt" state))
  
  (define-values (state+ cheated?) 
    (cond
      [(failed? choice-failed)
       (values (delete-player state avatar) #true)]
      [(legal-initial state choice-failed) => (λ (next) (values next #false))]
      [else
       (values (delete-player state avatar) #true)]))
  
  (values (next-player state+) cheated?))

(module+ test 
  (define (check-bad-init bad-player msg)
    (define st (create-state 2 9 (list player1 bad-player)))
    (define state11 (next-player (place-avatar st '[0 0])))
    (check-values (one-initial-turn state11)
                  (next-player (delete-player state11 (iplayer-color (second (fishes-players st)))))
                  #true
                  msg))

  (check-values (one-initial-turn state0) (next-player (place-avatar state0 '[0 0])) #f "good acts")
  (check-bad-init (new bad-init-time%) "time out for bad init time player")
  (check-bad-init (new bad-init-choice%) "time out for bad init choice player"))

;; ---------------------------------------------------------------------------------------------------
#; {State Posn -> (U False State)}
;; check the legality of the desired placement action for the active player,
;; construct resulting state including the rotation of player 
;; ASSUME the first player is the one that places an avatar 
(define (legal-initial state p)
  (define players (fishes-players state))
  (define places  (append-map iplayer-places players))
  (cond
    [(or (free-at state p) (member p places)) #false]
    [else (place-avatar state p)]))

(module+ test 
  (check-equal? (legal-initial state0 '[0 0]) (place-avatar state0 '[0 0]))
  (check-false (legal-initial state1 '[0 0]))
  (check-false (legal-initial state2 '[0 0])))

;                              
;                              
;                              
;                              
;    ;;;;  ;;;;  ;;;;;;   ;;;  
;   ;;  ;      ; ;  ;  ; ;;  ; 
;   ;   ;      ; ;  ;  ; ;   ;;
;   ;   ;   ;;;; ;  ;  ; ;;;;;;
;   ;   ;  ;   ; ;  ;  ; ;     
;   ;; ;;  ;   ; ;  ;  ; ;     
;    ;;;;   ;;;; ;  ;  ;  ;;;; 
;       ;                      
;    ;  ;                      
;     ;;

(define first-round #true)

#; {Tree Player* Observer* -> (list Rankings Player*)}
;; compute the outcome of a game, starting from the post-setup state 
(define (play-game tree0 player*0 (o*0 '()))
  (set! first-round #true)
  (let play ([tree tree0] [o* o*0] [player* player*0] [actions '()] [cheats '()])
    ;; for time management and enbugging
    (when (>= (length actions) (length player*)) (set! first-round #false))
    (cond
      [(final? tree) (list (node-current tree) cheats)]
      [else
       (tree-shape-assertion tree0 (reverse actions) tree) ;; for safety
       #;
       (spy `[calling ,(get-field me (first player*))])
       (define-values (t+ step o+) (play-turn tree (actions-since-last-turn actions player*) o*))
       (cond
         [step (play t+ o+ (rotate player*) (cons step actions) cheats)]
         [else (set! tree0 t+)
               (play t+ o+ (rest player*)   '[]                 (cons (first player*) cheats))])])))

#; {[Listof X] [Listof Y] -> [Listof X]}
(define (actions-since-last-turn actions0 player*)
  (if (<= (length actions0) (length player*)) '[] (reverse (take actions0 (- (length player*) 1)))))

#; {Tree [Listof Action] Tree -> Void}
;; does following the steps in `actions-from-tree0-to-tree` reach 'tree' from `tree0`?
(define (tree-shape-assertion tree0 actions-from-tree0-to-tree tree)
  (when tree0
    (define current-state  (node-current tree))
    (define computed       (apply tree-path tree0 actions-from-tree0-to-tree))
    (define computed-state (node-current computed))
    (unless (equal? computed-state current-state)
      (printf "comp: ~a\n" computed)
      (printf "actl: ~a\n" tree)
      (let-values ([(x _) (render-state computed-state)])
        (pretty-print `[computed state ,x]))
      (let-values ([(x _) (render-state current-state)])
        (pretty-print `[current state ,x]))
      (error 'tree-shape-assertion "assertion failure\n"))))

;; ---------------------------------------------------------------------------------------------------
(module+ test ; proper game scenarios
  (define turn-state0 (create-state 2 9 players-1-2-3 #:fixed 1))
  (define tree1 (generate-tree (first (initial-placements turn-state0 players-1-2-3))))
  (define tree1-step1 (take-action tree1 '[[0 0] [1 0]]))
  (define tree1-step2 (take-action tree1-step1 '[[0 1] [1 1]]))
  
  #;{Player [{0,1,2,3}] -> (values Player Player* Tree Tree)}
  ;; create a bad scenario from bad `player`, insert into `players-1-2-3` at position i
  ;; return the player, the game tree, and the expected result of one turn 
  (define (make-bad-set-up player (i 0))
    (define externals (append (take players-1-2-3 i) (list player) (drop players-1-2-3 i)))
    (define state0    (create-state 2 8 externals #:fixed 1))
    (define state1    (first (initial-placements state0 externals)))
    (define expected  (delete-player state1 (iplayer-color (list-ref (fishes-players state0) i))))
    (values externals (generate-tree state1) expected)))

(module+ test ; complete proper games

  (define (lengths t p)
    (define f (play-game t p))
    (list (length (fishes-players (first f))) (length (second f))))

  (check-equal? (lengths tree1 players-1-2-3) '[3 0] "a basic proper game")

  (let ()
    (define players (list bad-turn-choice player1))
    (define bad-state0 (create-state 2 8 players #:fixed 1))
    
    (check-equal? (lengths (generate-tree (first (initial-placements bad-state0 players))) players)
                  '[1 1]
                  "1 good, 1 bad for a game proper"))

  (let ()
    (define players (list bad-turn-choice bad-turn-time))
    (define bad-state0 (create-state 2 8 players #:fixed 1))
    
    (check-equal? (lengths (generate-tree (first (initial-placements bad-state0 players))) players)
                  '[0 2]
                  "2 bads for a game proper")))


;                              
;                              
;     ;                        
;     ;                        
;   ;;;;;  ;   ;   ;;;;  ; ;;  
;     ;    ;   ;   ;;  ; ;;  ; 
;     ;    ;   ;   ;     ;   ; 
;     ;    ;   ;   ;     ;   ; 
;     ;    ;   ;   ;     ;   ; 
;     ;    ;   ;   ;     ;   ; 
;     ;;;   ;;;;   ;     ;   ; 
;                              
;                              
;                              

#; {Tree [Listof Action] Observer* -> (values Tree (U Action False) Observer*)}
;; the active player takes a turn to get next tree, cheater (if), and surviving observers 
;; if legal, take a step in tree
;; if not, generate new tree after deleting the (first) player
(define (play-turn tree actions-taken-since-last (observers '()))
  (parameterize ([time-out-limit (if first-round (ready-time) (time-out-limit))])
    (cond
      [(noop? tree) (define a (caar (node-mapping tree))) (values (take-action tree a) a observers)]
      [else (play-proper-turn tree actions-taken-since-last observers)])))

#; {Tree [Listof Action] Observer* -> (values Tree (U Action False) Observer*)}
;; the active player is now guaranteed to be able to play properly;
;; protect against failure and cheaters only 
(define (play-proper-turn tree actions-taken-since-last observers)
  (match-define (node state mapping) tree)
  (define active   (if (and (not no-bug?) (not first-round))
                       (last (fishes-players state))
                       (first (fishes-players state))))
  (define external (iplayer-payload active))
  (define avatar   (iplayer-color active))
  
  (define choice-failed
    (parameterize ([time-out-limit (time-out-limit)])
      (xsend external take-turn #:caller "referee.rkt" state actions-taken-since-last)))
  
  (define-values (msg state+ act)
    (cond
      [(failed? choice-failed)
       (define msg (~a avatar " failed: " (failed-value choice-failed)))
       (values msg (generate-tree (delete-player state avatar)) #false)]
    
      [(take-action tree choice-failed)
       => (λ (t) (values (node-current t) t choice-failed))]
    
      [else
       (define from (first choice-failed))
       (define to   (second choice-failed))
       (define msg  (~a avatar " attempted to cheat, trying to move from " from " to " to))
       (values msg (generate-tree (delete-player state avatar)) #false)]))

  (define observers+  (xinform-observers observers state (if (string? msg) msg (list act msg))))
  (values state+ act observers+))

(module+ test
  (define-syntax-rule (2val e) (let-values ([(x y z) e]) (values (node-current x) y z)))
  
  (check-values (2val (play-turn tree1 '[]))
                (node-current tree1-step1)
                '[[0 0] [1 0]]
                '[]
                "the very first turn")
  
  (check-values (2val (play-turn tree1-step1 '[ [[0 0] [1 0]] ]))
                (node-current tree1-step2)
                '[[0 1] [1 1]]
                '[]
                "the second turn")
  
  (let-values ([(_ bad-tree bad-expected) (make-bad-set-up bad-turn-choice)])
    (check-values (2val (play-turn bad-tree '[])) bad-expected #f '[]
                  "the bad action guy goes first"))

  (let-values ([(_ bad-tree bad-expected) (make-bad-set-up bad-turn-time)])
    (check-values (2val (play-turn bad-tree '[])) bad-expected #f '[]
                  "the bad timing guy goes first")))

;                                                                 
;          ;                                                      
;          ;                                                      
;          ;                                                      
;    ;;;   ;;;;    ;;;    ;;;    ;;;;  ;   ;   ;;;    ;;;;   ;;;  
;   ;; ;;  ;; ;;  ;   ;  ;;  ;   ;;  ; ;   ;  ;;  ;   ;;  ; ;   ; 
;   ;   ;  ;   ;  ;      ;   ;;  ;      ; ;   ;   ;;  ;     ;     
;   ;   ;  ;   ;   ;;;   ;;;;;;  ;      ; ;   ;;;;;;  ;      ;;;  
;   ;   ;  ;   ;      ;  ;       ;      ; ;   ;       ;         ; 
;   ;; ;;  ;; ;;  ;   ;  ;       ;       ;    ;       ;     ;   ; 
;    ;;;   ;;;;    ;;;    ;;;;   ;       ;     ;;;;   ;      ;;;  
;                                                                 
;                                                                 
;                                                                 

#; {[Listof Observer] (U [List Action State] String) -> [Listof Observer]}
;; inform observers about the current turn; produce all those that interact properly 
(define (xinform-observers observers0 state turn-legal)
  (let loop ([observers observers0][broken '[]])
    (cond
      [(empty? observers) (remove* broken observers0)]
      [else 
       (define o1 (first observers))
       (define void-failed (o1 state turn-legal))
       (if (failed? void-failed)
           (loop (remove o1 (rest observers)) (cons o1 broken))
           (loop (rest observers) broken))])))

;; ---------------------------------------------------------------------------------------------------
(module+ picts ;; duplicates players so I don't have to import `test` to watch a game
  
  (parameterize ([explore-to-depth 1] [sleep-time .1])
    (referee (create-state 3 3 one-good-one-bad) #:observers (list observer))
    (void))
  
  (parameterize ([explore-to-depth 2] [sleep-time .2])
    (referee (create-state 5 5 all-imperative #:holes '[[0 0] [3 3]]) #:observers (list observer))
    (void)))

;                                                  
;                                                  
;              ;               ;                   
;     ;                                            
;     ;                                            
;   ;;;;;;   ;;;    ;;;;;;   ;;;    ; ;;;    ;;; ; 
;     ;        ;    ;  ;  ;    ;    ;;   ;  ;;  ;; 
;     ;        ;    ;  ;  ;    ;    ;    ;  ;    ; 
;     ;        ;    ;  ;  ;    ;    ;    ;  ;    ; 
;     ;        ;    ;  ;  ;    ;    ;    ;  ;    ; 
;     ;        ;    ;  ;  ;    ;    ;    ;  ;;  ;; 
;      ;;;   ;;;;;  ;  ;  ;  ;;;;;  ;    ;   ;;; ; 
;                                                ; 
;                                            ;  ;; 
;                                             ;;;  
;                                                  

(module+ profile
  ; 10 x 10 means one time out 
  (profile (referee (create-state 10 10 all-imperative #:holes '[[3 3]])))
  (profile (referee (create-state 10 10 players-1-2-3 #:holes '[[3 3]]))))

(module+ time
  (define-syntax-rule
    (dfenv x d)
    (define x (let ([x (getenv (~a 'x))]) (or (and x (string->number x)) d))))

  (dfenv TPC 20)
  (dfenv EXP 2)
  (dfenv ROW 5)
  (dfenv COL 5)

  (displayln `[timing ,ROW x ,COL board with exploration depth ,EXP for ,TPC s/call])  

  '**imperative**
  (collect-garbage) (collect-garbage) (collect-garbage)
  (parameterize ([explore-to-depth EXP] [time-out-limit TPC])
    (time (referee (create-state ROW COL all-imperative #:holes '[[3 3]]))))
  ; 10 x 10 means one time out
  ;; cpu time: 4368 real time: 4448 gc time: 1065

  '**functional**
  (collect-garbage) (collect-garbage) (collect-garbage)
  (parameterize ([explore-to-depth EXP] [time-out-limit TPC])
    (time (referee (create-state ROW COL players-1-2-3 #:holes '[[3 3]]))))
  ; 10 x 10 means one time out
  ;; cpu time: 7400 real time: 7543 gc time: 1406
  )
