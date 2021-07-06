#lang racket

;; TODO: eliminate ROW/COLUMN constants from below and allow players to set up dimensions

;; an interactive human player

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

(provide
 (contract-out
  ;; a player mechanicsm that interacts with a person for initial placements and turns 
  [human% player%/c]))

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

(require Fish/GUI/game-view)
(require Fish/Player/game-control)
(require Fish/Common/board)
(require Fish/Common/game-state)
(require Fish/Common/internal-player)
(require (only-in racket/gui message-box current-eventspace make-eventspace))

;                                          
;                                          
;   ;                                      
;   ;                                      
;   ;                                      
;   ; ;;;   ;    ;  ;;;;;;    ;;;   ; ;;;  
;   ;;   ;  ;    ;  ;  ;  ;  ;   ;  ;;   ; 
;   ;    ;  ;    ;  ;  ;  ;      ;  ;    ; 
;   ;    ;  ;    ;  ;  ;  ;  ;;;;;  ;    ; 
;   ;    ;  ;    ;  ;  ;  ; ;    ;  ;    ; 
;   ;    ;  ;   ;;  ;  ;  ; ;   ;;  ;    ; 
;   ;    ;   ;;; ;  ;  ;  ;  ;;; ;  ;    ; 
;                                          
;                                          
;                                          
;                                          

(define ROWS 8)
(define COLUMNS 8)

(define human%
  (class object% (init-field (strategy 'just-to-satisfy-the-contract))

    ;; -----------------------------------------------------------------------------------------------
    ;; tournament methods 
    (define/public (start-of-tournament go)
      (void
       (message-box "tournament status" "starting the tournament")))

    (define/public [end-of-tournament win-or-lose]
      (void
       (message-box  "tournament status" (if win-or-lose "you won" "you lost") #f '(ok stop))))

    ;; -----------------------------------------------------------------------------------------------
    ;; game methods 
    (field
     [my-color "green"]
     [my-view  #false])

    (define/public [playing-as c]
      (set! my-color c))

    (define/public [playing-with others]
      (void))

    (define/public (initial state)
      (define players  (fishes-players state))
      (define myavatar (iplayer-penguin (first players)))
      (define places   (append-map iplayer-places players))
      (define board    (fishes-board state))
      (define-values (board-pict score-pict) (render-state state #:color my-color))
      (unless my-view
        (parameterize ([current-eventspace (make-eventspace)])
          (set! my-view (new game-frame% [board0 board-pict] [score0 score-pict]))))
      (send my-view show #t)
      (define (pick-place c)
        (new initial-control% [result-channel c] [gui->model (ok? board places)]))
      (wait (λ (ch) (send my-view choose board-pict score-pict myavatar (pick-place ch)))
            state
            place-avatar))

    (define/public [take-turn state actions]
      (define players   (fishes-players state))
      
      (define myavatar  (iplayer-penguin (first players)))
      (define myplaces  (iplayer-places (first players)))
      (define myavatars (map (λ (p) (list myavatar p (logic-posn->pict-center p))) myplaces))

      (define places    (append-map iplayer-places (rest players)))
      (define board     (fishes-board state))
      (define-values (board-pict score-pict)
        (render-state state #:color my-color #:remove-places-of-first-player #t))
      (define (pick-move ch)
        (define (ok-step? from to)
          ;; ASSUME players are not called when they can't move 
          (member (list from to) (all-possible-actions state)))
        (new tt-control% [result-channel ch] [ok-step? ok-step?] [gui->model (ok? board places)]))
      (wait (λ (ch) (send my-view take-turn board-pict score-pict myavatars (pick-move ch)))
            state
            (λ (state action) (apply move-avatar state action))))

    #;{[Channel -> Void] State [State X -> State] -> X}
    ;; compute the choice that the human player makes 
    ;; spawn `present-choices-in-view` as thread, wait for result-channel, get & present next state 
    (define/private (wait present-choices-in-view state process-result)
      (let wait ()
        (define result-ch (make-channel))
        (present-choices-in-view result-ch)
        (define result (channel-get result-ch))
        (cond
          [(boolean? result) (wait)]
          [else 
           (define state++ (process-result state result))
           (define-values (board++ score++) (render-state state++ #:color my-color))
           (send my-view show-state board++ score++)
           result])))

    (super-new)))

#; {Board [Listof Posn] -> [Posn -> Posn Posn]}
;; ask whether placing an avatar or moving there is okay 
(define ((ok? board places) gui-posn)
  (define-values (candidate-row-column candidate-center)
    (any-pict-posn->locic-center gui-posn #:rows ROWS #:columns COLUMNS))
  (if (and
       candidate-row-column
       (not (member candidate-row-column places))
       (> (fish-at board candidate-row-column) 0))
      (values candidate-row-column candidate-center)
      (values #false               #false)))

;; ---------------------------------------------------------------------------------------------------
(module+ test1
  (define human (new human%))
  ; (send human start-of-tournament #true)
  ; (send human end-of-tournament #false)

  (define state0 (create-state 4 5 '("suzanne" "kanika" "julia")))
  (define state1
    (let* ([s state0]
           [s (next-player (place-avatar s '[0 0]))]
           [s (next-player (place-avatar s '[0 1]))]
           [s (next-player (place-avatar s '[0 2]))])
      s))
  (define my-color (iplayer-color (first (fishes-players state0))))
  (define other-colors (map iplayer-color (rest (fishes-players state0))))
  (send human playing-as my-color)
  (define pick (send human initial state1))
  (define state2 (place-avatar state1 pick))
  (send human initial state2))

(module+ picts
  (define human (new human%))
  ; (send human start-of-tournament #true)
  ; (send human end-of-tournament #false)

  (define state0 (create-state 4 5 '("suzanne" "kanika" "julia")))
  (define state1
    (let* ([s state0]
           [s (next-player (place-avatar s '[0 0]))]
           [s (next-player (place-avatar s '[0 1]))]
           [s (next-player (place-avatar s '[0 2]))])
      s))
  (define my-color (iplayer-color (first (fishes-players state0))))
  (define other-colors (map iplayer-color (rest (fishes-players state0))))
  (send human playing-as my-color)
  (define pick (send human initial state1))
  (define state2 (place-avatar state1 pick))
  (send human take-turn state2 '[]))
