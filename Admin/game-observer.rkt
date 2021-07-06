#lang racket/gui

;; a simple game observer

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

(require (only-in Fish/Common/player-interface game-observer/c))

(provide
 sleep-time ;; parameter[natural]

 (contract-out
  (observer game-observer/c)))

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
(require Fish/GUI/gcanvas)
(require (except-in Fish/Common/player-interface game-observer/c))
(require Fish/Common/game-state)

;                                                                  
;                                                                  
;           ;                                                      
;           ;                                                      
;           ;                                                      
;    ;;;;   ; ;;;    ;;;;    ;;;;    ;;;;   ;    ;   ;;;;    ;;;;  
;   ;;  ;;  ;;  ;;  ;    ;  ;    ;   ;;  ;  ;;  ;;  ;    ;   ;;  ; 
;   ;    ;  ;    ;  ;       ;;;;;;   ;       ;  ;   ;;;;;;   ;     
;   ;    ;  ;    ;   ;;;;   ;        ;       ;  ;   ;        ;     
;   ;    ;  ;    ;       ;  ;        ;       ;;;;   ;        ;     
;   ;;  ;;  ;;  ;;  ;    ;  ;;   ;   ;        ;;    ;;   ;   ;     
;    ;;;;   ; ;;;    ;;;;    ;;;;;   ;        ;;     ;;;;;   ;     
;                                                                  
;                                                                  
;                                                                  
;                                                                  

(define sleep-time (make-parameter SLEEP))

#; {State -> (State Turn [U False State] -> Void)}
#; {observer : game-observer/c}
(define (observer state0)
  (define es (make-eventspace))
  (define-values (board-pict score-pict) (render-state state0))
  (parameterize ([current-eventspace es])
    (define gf (new game-frame% [board0 board-pict] [score0 score-pict] [title "OBSERVER"]))
    (send gf show #t)
    (callback es gf)))
  
;; ---------------------------------------------------------------------------------------------------
#; {EventSpace [Instance GameFrame] -> [State (U String [List Action State]) -> Void]}
;; create a callback function that separately shows
;; -- the score resulting from this move and order of play,
;; -- the board with the penguins and the move 
(define ((callback es gf) state legal-action)
  (parameterize ([current-eventspace es])

    (define-values (fall-asleep msg state-pict score-pict)
      (cond
        [(string? legal-action)
         (define-values (a b) (render-state state))
         (values (* 4 (sleep-time)) msg a b)]
        [else
         (match-define (list action state+) legal-action)
         (define-values (state0-pict score0-pict) (render-state state+))
         (values (sleep-time) "" state0-pict score0-pict)]))
    (queue-callback (λ () (send gf show-state state-pict score-pict #:text msg)))
    (sleep fall-asleep)))

;; ---------------------------------------------------------------------------------------------------
(module+ picts
  (observer (create-state 3 3 '("suzanne" "kanika" "julia")))

  (observer (create-state 5 8 '("jason" "matthias" "evan"))))
