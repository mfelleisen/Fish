#lang racket

;; represent the game state for "Fish.com"
;; -- the "physical" board
;; -- the state of the players 
;; -- the order in which players proceed 

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

(require (only-in Fish/Common/board posn/c board/c))
(require (only-in Fish/Common/internal-player iplayer? iplayer-color))
(require (only-in pict pict?))

(define SKIP 'skip)
(define (skip? x) (eq? SKIP x))
(define move/c (list/c posn/c posn/c))
(define turn? (or/c SKIP move/c))

(require Fish/Common/fish)
(require (only-in Fish/Common/penguin penguin-color/c))

(provide

 PENGUIN-N

 turn? ;; is one of: 
 skip?
 move/c
 #; {Board Stepper [N Posn -> (U False X)] -> [Listof X]}
 state-board-traverse

 (contract-out

  (fishes? contract?)
  
  (fishes-players (-> fishes? [listof iplayer?]))
  (fishes-current-player (-> fishes? penguin-color/c))
  (fishes-board (-> fishes? board/c))
   
  (create-state
   ;; create an initial state for n players and allocate n colors, optionally with fixed # of fish
   (->i ([r# natural-number/c] [c# natural-number/c] [lox (listof any/c)])
        [#:fixed (f fish#/c) #:holes (h (listof posn/c))]
        [_ fishes?]))

  (complete-state
   ;; adds tiles at bottom and place penguins on it to make sure every player has the proper penguins
   (-> fishes? fishes?))

  (assigned-penguins (-> list? natural?))

  (score-of
   ;; determine the score of the specifid player in this state, #false if the player is not playing
   (-> penguin-color/c fishes? (or/c #false natural?)))

  (next-player
   ;; proceed in player order and report which color gets to play next in this state and the new state
   (-> fishes? fishes?))

  (free-at
   (-> fishes? posn/c boolean?))
  
  (place-avatar
   ;; place avatar for specified player at given position in this state 
   (-> fishes? posn/c fishes?))

  (move-avatar
   ;; the first player in this state moves an avatar from the old to the new & cash in fish
   (->i ([s fishes?] [old posn/c] [new posn/c]) [r fishes?]))
  
  (all-possible-actions 
   ;; list all possible actions that the current player in this state may take:
   ;; -- '[]    :: a final state, the active player and no other player can make a move
   ;; -- SKIP   :: a state in which the active player can't move but another one can
   ;; -- a list :: all moves the active player may make 
   (->i ([f fishes?]) #:pre/name (f) "there are players" (cons? (fishes-players f))
        (r (or/c skip? (listof move/c)))))

  (delete-player
   ;; remove the specified color from this state
   (->i ([f fishes?] [p penguin-color/c])
        #:pre/name (f p) "player exists in state" (member p (map iplayer-color (fishes-players f)))
        (r fishes?)))

  (render-state
   (->i ([s fishes?])
        (#:arrow (arrow (list/c posn/c posn/c))
         #:color (color string?)
         #:remove-places-of-first-player (b boolean?))
        (values [board pict?] [players pict?])
        #; {#:post/name (board players) "same height" (= (pict-height board) (pict-height players))}))

  ;; for debugging 
  (show (-> any/c fishes? any))))

(module+ examples
  (provide
   ;; basic states 
   00-state 11-state 22-state
   ;; additional for game actions 
   2-state-no-action 2-state-1-action
   basic-state basic-board basic-players))

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

(require (except-in Fish/Common/penguin penguin-color/c))
(require (except-in Fish/Common/board posn/c))
(require (except-in Fish/Common/internal-player iplayer? iplayer-color))

(require (except-in pict pict?))

(module+ examples  
  (require (submod "..")))

(module+ test
  (require (submod ".." examples))
  (require (submod ".."))
  (require rackunit))

(module+ serialize
  (require SwDev/Lib/pattern-matching))

;                                                                          
;                                                                          
;        ;                                                                 
;        ;            ;                                                    
;        ;            ;                                                    
;    ;;; ;    ;;;   ;;;;;;    ;;;            ;;;;    ;;;;   ; ;;;          
;   ;;  ;;   ;   ;    ;      ;   ;           ;;  ;  ;    ;  ;;  ;;         
;   ;    ;       ;    ;          ;           ;      ;;;;;;  ;    ;         
;   ;    ;   ;;;;;    ;      ;;;;;           ;      ;       ;    ;         
;   ;    ;  ;    ;    ;     ;    ;           ;      ;       ;    ;    ;;   
;   ;;  ;;  ;   ;;    ;     ;   ;;           ;      ;;   ;  ;;  ;;    ;;   
;    ;;; ;   ;;; ;     ;;;   ;;; ;           ;       ;;;;;  ; ;;;     ;;   
;                                                           ;              
;                                                           ;              
;                                                           ;              
;                                                                          

(define PENGUIN-N   6) ;; the number of penguins per player is determine as PENGUIN-N - #players

(struct fishes [board players] #:prefab)
#; {type Fishes  = (fishes Board [Listof InternalPlayer])}
;; INTERPRETATION the state combines the current board and the sequence of players still in play
;; the sequence is ordered according to the turn order

(define (create-state rows columns lox #:fixed (f #f) #:holes (h '[]))
  (define n (length lox))
  (define board    (if f (make-board rows columns #:fixed f #:- h) (make-board rows columns #:- h)))
  (define iplayers (for/list {[x lox] [cp penguins]} (create-player cp x)))
  (fishes board iplayers))

(define (assigned-penguins players) (- PENGUIN-N (length players)))

(define (proper-player# players) (<= 2 (length players) 4)) ;; see Admin/basics

(define (fishes-current-player f)
  (iplayer-color (first (fishes-players f))))

;; ---------------------------------------------------------------------------------------------------
(define (complete-state s)
  (define players  (fishes-players s))
  (define penguins (map iplayer-places players))
  (define needed   (assigned-penguins players))
  (cond
    [(apply = needed (map length penguins)) s]
    [else (+penguins-and-rows (fishes-board s) players penguins needed)]))

#;{ Board Players [Listof Posn] N -> Fishes}
(define (+penguins-and-rows board players penguins needed)
  (define width   (board-columns board))
  (define board++ (add-row (add-row board width #false) width #false))
  (define height  (board-rows board++))
  (define players++
    (for/list ([p players] [1penguin penguins])
      (define penguin# (length 1penguin))
      (cond
        [(= penguin# needed) p]
        [else
         (let add-rows-and-place-penguins ([p p][this-player-needs (- needed penguin#)])
           (define penguin# (min this-player-needs width))
           (set!-values (board++ height) (values (add-row board++ penguin# 1) (+ height 1)))
           (define penguins (build-list penguin# (λ (j) (list (- height 1) j))))
           (define need++   (- this-player-needs penguin#))
           (define p++ (for/fold ([p p]) ([n penguins]) (+place-player p n)))
           (if (<= need++ 0) p++ (add-rows-and-place-penguins p++ need++)))])))
  (fishes board++ players++))

(module+ test
  (define players (list (create-player (first penguins) 'pl) (create-player (second penguins) 'pl)))
  (define board   (make-board 2 2 #:fixed 1))
  (define state   (complete-state (fishes board players)))

  (check-equal? (length (iplayer-places (first (fishes-players state)))) (assigned-penguins players))
  (check-equal? (length (iplayer-places (cadr (fishes-players state)))) (assigned-penguins players)))

;; ---------------------------------------------------------------------------------------------------
(define (score-of color s)
  (match-define (fishes board players) s)
  (for*/first ([p players] [c (in-value (iplayer-color p))] #:when (equal? color c))
    (iplayer-score p)))

;; ---------------------------------------------------------------------------------------------------
(define (next-player s)
  (define players (rotate (fishes-players s)))
  (struct-copy fishes s [players players]))

#; {[Listof X] -> [Listof X]}
(define (rotate l)
  (if (empty? l) l (append (rest l) (list (first l)))))

;; ---------------------------------------------------------------------------------------------------
(define (free-at state p)
  (= (fish-at (fishes-board state) p) 0))

(define (place-avatar s place)
  (define players (fishes-players s))
  (define p (first players))
  (struct-copy fishes s [players (cons (+place-player p place) (rest players))]))

;; ---------------------------------------------------------------------------------------------------
(define (move-avatar s old to)
  (match-define (fishes board players) s)
  (define p (first players))
  (define-values (n new-board) (remove-tile board old))
  (define new-players (cons (upscore-player (move-player p old to) n) (rest players)))
  (fishes new-board new-players))

;; ---------------------------------------------------------------------------------------------------
(define (all-possible-actions s)
  (match-define (fishes board players) s)
  (define avatar-per-player (map iplayer-places players))
  (define blocked           (apply append avatar-per-player))
  (define first-moves 
    (for/fold ([r '()]) ([p (first avatar-per-player)])
      (define reachable (all-possible board p #:reserved blocked))
      (define from-to   (map (λ (r) (list p r)) reachable))
      (append from-to r)))
  (if (and (empty? first-moves) (movable? s)) SKIP first-moves))

(define (movable? s)
  (let/ec return 
    (match-define (fishes board players) s)
    (define avatar-per-player (map iplayer-places players))
    (define all-avatars       (apply append avatar-per-player))
    (for ([a avatar-per-player])
      (for* ([p a] [r (in-value (all-possible board p #:reserved all-avatars))] #:when (cons? r))
        (return #true)))
    #false))

;; ---------------------------------------------------------------------------------------------------
(define (delete-player s p)
  (struct-copy fishes s [players (remf (λ (q) (equal? (iplayer-color q) p)) (fishes-players s))]))

;; ---------------------------------------------------------------------------------------------------
(module+ examples
  
  (define basic-state (create-state 4 5 '(x y)))
  (match-define (fishes basic-board basic-players) basic-state)

  (define 00-state (place-avatar basic-state '[0 0]))

  (define 11-state
    (let*-values ([(state) (create-state 2 1 '(x y) #:fixed 2)]
                  [(state) (place-avatar state '[0 0])]
                  [(state) (next-player (place-avatar state '[1 0]))])
      state))

  (define 22-state
    (let*-values ([(state) (create-state 2 2 '(x y) #:fixed 2)]
                  [(state) (place-avatar state '[0 0])]
                  [(state) (next-player (place-avatar state '[1 0]))]
                  [(state) (next-player (place-avatar state '[0 1]))])
      state))

  (define 2-state-no-action (create-state 2 3 '(x y z) #:fixed 1))
  (define 2-state-1-action (place-avatar 2-state-no-action '[0 0])))

;                                                                          
;                                                                          
;                                                                    ;;;   
;     ;                                                                ;   
;     ;                                                                ;   
;   ;;;;;;   ;;;;     ;;;   ;    ;   ;;;;    ;;;;    ;;;;     ;;;      ;   
;     ;      ;;  ;   ;   ;  ;;  ;;  ;    ;   ;;  ;  ;    ;   ;   ;     ;   
;     ;      ;           ;   ;  ;   ;;;;;;   ;      ;            ;     ;   
;     ;      ;       ;;;;;   ;  ;   ;        ;       ;;;;    ;;;;;     ;   
;     ;      ;      ;    ;   ;;;;   ;        ;           ;  ;    ;     ;   
;     ;      ;      ;   ;;    ;;    ;;   ;   ;      ;    ;  ;   ;;     ;   
;      ;;;   ;       ;;; ;    ;;     ;;;;;   ;       ;;;;    ;;; ;      ;;;
;                                                                          
;                                                                          
;                                                                          
;                                                                          

(define (state-board-traverse s in-which-order f)
  (define players (fishes-players s))
  (define board   (fishes-board s))
  (board-traverse board in-which-order f #:reserved (append-map iplayer-places players)))

;                                                                          
;                                                                          
;                                ;                     ;                   
;                                ;                                         
;                                ;                                         
;    ;;;;    ;;;;   ; ;;;    ;;; ;   ;;;;    ;;;;    ;;;    ; ;;;    ;;; ; 
;    ;;  ;  ;    ;  ;;   ;  ;;  ;;  ;    ;   ;;  ;     ;    ;;   ;  ;;  ;; 
;    ;      ;;;;;;  ;    ;  ;    ;  ;;;;;;   ;         ;    ;    ;  ;    ; 
;    ;      ;       ;    ;  ;    ;  ;        ;         ;    ;    ;  ;    ; 
;    ;      ;       ;    ;  ;    ;  ;        ;         ;    ;    ;  ;    ; 
;    ;      ;;   ;  ;    ;  ;;  ;;  ;;   ;   ;         ;    ;    ;  ;;  ;; 
;    ;       ;;;;;  ;    ;   ;;; ;   ;;;;;   ;       ;;;;;  ;    ;   ;;; ; 
;                                                                        ; 
;                                                                    ;  ;; 
;                                                                     ;;;  
;                                                                          

(define (show tag s)
  (define-values (p x) (render-state s))
  (pretty-print `[,tag ,(hc-append 10 p x)] (current-error-port)))

(define (render-state s #:arrow (arrow #f) #:color (color #f) #:remove-places-of-first-player (b #f))
  (match-define (fishes board players) s)
  (define penguin-places (ready-penguin-for-places (if b (rest players) players)))
  (define bd
    (cond
      [(and color arrow) (render-board board #:+ penguin-places #:arrow arrow #:color color)]
      [color (render-board board #:+ penguin-places #:color color)]
      [else  (render-board board #:+ penguin-places)]))
  (define p* (render-player-scores players (pict-height bd)))
  (values bd p*))

#; {[Listof InternalPlayer] -> [Listof [Cons Pict [Listof Posn]]]}
;; combine internal player's avatar with its position 
(define (ready-penguin-for-places players)
  (for/list ([p players])
    (cons (colored (iplayer-penguin p) (iplayer-color p)) (iplayer-places p))))

#; {Pict ColorString -> Pict}
;; put the avatar on a color rectangle of the player's color 
(define (colored pict c)
  ;; this line is a kludge and will need adjustment if I find penguins 
  (define r (filled-rectangle (pict-width pict) (- (pict-height pict) 9) #:color c #:draw-border? #f))
  (cc-superimpose r pict))

#; {[Listof InternalPlayer] N -> Pict}
;; render the scores of the players in approximately the height of the board 
(define (render-player-scores players height)
  (define n (length players))
  (define scores (map (λ (p) (text (~a (iplayer-score p)) 'roman 22)) players))

  (define player-height (+ (apply max 20 (map pict-height scores)) 2))
  (define total-height  (* n player-height))
  (when (> total-height height)
    (define ce (current-error-port))
    (log-info "render-players: something's wrong\n players ~a total ~a\n" total-height height))
  (define DELTA (if (= n 1) height (quotient (- height total-height) (- n 1))))
  (define h (if (= n 0) height (quotient (- height (* (- n 1) DELTA)) n)))
  (define picts
    (for/list ([p players])
      (define c (iplayer-color p))
      (define s (iplayer-score p))
      (cc-superimpose
       (filled-rectangle 100 h #:color c)
       (colorize (text (~a s) 'roman 22) (if (equal? "white" c) "black" "white")))))
  (apply vl-append DELTA picts))

;; ---------------------------------------------------------------------------------------------------
(module+ picts
  (require (submod ".." examples))

  (render-state 00-state)
  (render-state 00-state #:remove-places-of-first-player #t)
  
  (render-state 11-state)
  
  (render-state 22-state)

  (render-state 22-state #:arrow '[[0 0] [0 1]])

  (define state (create-state 2 3 '(x y z) #:fixed 1))
  (render-state state)
  (define broken-state (place-avatar state '[0 0]))
  (render-state broken-state))

;                                          
;                                          
;                                          
;     ;                       ;            
;     ;                       ;            
;   ;;;;;;   ;;;;    ;;;;   ;;;;;;   ;;;;  
;     ;     ;    ;  ;    ;    ;     ;    ; 
;     ;     ;;;;;;  ;         ;     ;      
;     ;     ;        ;;;;     ;      ;;;;  
;     ;     ;            ;    ;          ; 
;     ;     ;;   ;  ;    ;    ;     ;    ; 
;      ;;;   ;;;;;   ;;;;      ;;;   ;;;;  
;                                          
;                                          
;                                          
;                                          

(module+ test
  
  (check-equal? (next-player basic-state) (fishes basic-board (rotate basic-players)) "next player")

  (check-equal? (fishes-players (delete-player basic-state (iplayer-color (second basic-players))))
                (list (first basic-players))
                "delete player")
  
  (define p (first basic-players))
  
  (check-equal? (iplayer-places (+place-player p '[0 0])) '[[0 0]] "place-avatar-players")
  
  (check-equal? (place-avatar basic-state '[0 0])
                (fishes basic-board (list (+place-player p '[0 0]) (second basic-players)))
                "place-avatar")
  
  (check-false (movable? basic-state))

  ;; -----------------------------------------------------------------------------
  
  (match-define (fishes 00-board 00-players) 00-state)
  
  (check-equal? (move-avatar 00-state '[0 0] '[1 1])
                (let*-values ([(n b) (remove-tile 00-board '[0 0])]
                              [(n1) (upscore-player (move-player (first 00-players) '[0 0] '[1 1]) n)]
                              [(old) (second 00-players)])
                  (fishes b (list n1 old)))
                "take turn")


  (check-true (movable? 00-state))
  
  (check-equal? (fishes-players (delete-player 11-state "white"))
                (rest (fishes-players 11-state))
                "delete player, retain places")

  (check-false (movable? 11-state))
  
  ;; -----------------------------------------------------------------------------
  (check-equal? (all-possible-actions 00-state)
                '(((0 0) (1 0)) ((0 0) (2 1)) ((0 0) (3 1)) ((0 0) (2 0)))
                "all-possible-actions")
  (check-equal? (all-possible-actions 11-state) '[] "no possible-actions")
  
  (check-equal? (all-possible-actions 22-state) SKIP))

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

  (require (only-in json jsexpr?))

  (provide
   PLAYERS

   (contract-out
    (state         (->i ([j jsexpr?]) (#:soft (s boolean?)) (r (or/c #false fishes?))))
    (state->jsexpr (-> fishes? jsexpr?))))

  (require (submod Fish/Common/board serialize))
  (require (submod Fish/Common/internal-player serialize))
  
  (define PLAYERS 'players)
  (def/mp state/pat [_ b p]
    #'(hash-table
       (BOARD (? board-validator b))
       (PLAYERS (list (? player-validator p) (... ...)))))

  #; {JSexpr -> Fishes}
  (define (state j #:soft (soft #f))
    (let/ec return
      (match j
        [(state/pat b p)
         (define board (jsexpr->board b))
         (define players (map jsexpr->player p))
         (define penguins  (map iplayer-places players))

         (unless soft
           (unless (<= 2 (length players) 6)
             (displayln `[wrong number of players (,(length players))] (current-error-port))
             (return #false))
		    
           (unless (apply = (assigned-penguins players) (map length penguins))
             (displayln "wrong number of penguins for given number of players" (current-error-port))
             (return #false)))
    
         (define positions (apply append penguins))
         (unless (and (= (set-count (apply set positions)) (length positions))
                      (for/and ([p positions]) (> (fish-at board p) 0)))
           (displayln "something's wrong with penguin positions" (current-error-port))
           (return #false))
       
         (fishes board players)]
        [j #false])))
  
  (define (state->jsexpr f)
    (match-define (fishes board players) f)
    (make-hasheq `([,BOARD . ,(board->jsexpr board)] [,PLAYERS . ,(map player->jsexpr players)]))))
