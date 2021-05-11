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

(require Fish/Admin/gcanvas)
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
  (define-values (pict0 score0) (render-state state0))
  (parameterize ([current-eventspace es])
    (define frame
      (new frame% [label "game observer"] [stretchable-width #false] [stretchable-height #false]))
    (define panel
      (new horizontal-panel% [parent frame] [border 5] [style '(border)]))
    (define fcan
      (new gcanvas% [parent (vp panel)] [pict0 pict0]))
    (define scan
      (new gcanvas% [parent (vp panel)] [pict0 score0]))
    (define text
      (new text-field% [parent frame] [label #false] [font menu-control-font]))
    (send frame show #t)
    (callback es fcan scan text)))

;; ---------------------------------------------------------------------------------------------------
#; {EventSpace [Instance GCanvas] [Instance GCanvas] [Instance Text]
               ->
               [State (U String [List Action State]) -> Void]}
;; create a callback function that separately shows
;; -- the score resulting from this move and order of play,
;; -- the board with the penguins and the move 
(define ((callback es fcan scan text) state legal-action)
  (parameterize ([current-eventspace es])

    (define-values (state-pict score-pict)
      (cond
        [(string? legal-action)
         (send text set-value legal-action)
         (render-state state)]
        [else
         (match-define (list action state+) legal-action)
         ;; this is model knowledge: design bug?
         (define-values (state0-pict score0-pict)
           (if (skip? action) (render-state state) (render-state state #:arrow action)))
         (send text set-value "")
         (render-state state+)]))
    
    (send fcan set state-pict)
    (send scan set score-pict)
    
    (sleep (if (string? legal-action) (* 4 (sleep-time)) (sleep-time)))))

#; {Window -> [Instance VerticalPanel]}
(define (vp parent)
  (new vertical-panel% [parent parent] [border 5] [style '(border)]))

;; ---------------------------------------------------------------------------------------------------
(module+ picts
  (observer (create-state 3 3 '("suzanne" "kanika" "julia")))

  (observer (create-state 5 8 '("jason" "matthias" "evan"))))
