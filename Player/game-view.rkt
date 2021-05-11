#lang racket/gui

;; a view for a human player mechanism 

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

(require (only-in Fish/Player/game-control initial/c turn/c))
(require (only-in Fish/Common/board posn/c))
(require (only-in pict pict?))

(provide
 (contract-out 
  [game-frame%
   (class/c
    [choose
     ;; given a board, a score, an avatar and a controller for initial placements,
     ;; interact with the user to retrieve the place where the next avatar is to be placed 
     (->m pict? pict? pict? [instanceof/c initial/c] any)]
    
    (take-turn
     ;; given a board, a score, a list of avatar (with their logical posns and centers),
     ;; and a controller for turns: interact with the user to retrieve the next avatar-move action 
     (->m pict? pict? [listof (list/c pict? posn/c posn/c)] [instanceof/c turn/c] any))
    
    (show-state
     ;; given a board and a score, show the current game state 
     (->m pict? pict? any/c)))]))

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

(require Fish/Player/game-pasteboard)
(require Fish/Player/game-image-snip)
(require Fish/Admin/gcanvas)
(require (only-in Fish/Common/board posn-row posn-column))
(require Fish/Lib/toint)
(require (except-in pict pict?))

;                                  
;                                  
;              ;                   
;                                  
;                                  
;   ;    ;   ;;;     ;;;;  ;      ;
;   ;;  ;;     ;    ;    ; ;      ;
;    ;  ;      ;    ;;;;;;  ; ;; ; 
;    ;  ;      ;    ;       ; ;; ; 
;    ;;;;      ;    ;       ; ;; ; 
;     ;;       ;    ;;   ;   ; ;;  
;     ;;     ;;;;;   ;;;;;   ;  ;  
;                                  
;                                  
;                                  
;                                  

(define TURN-C "pink")

(define TITLE  "Hey, don't take my fish")
(define CHOOSE "move penguin from the bottom left to an unoccupied tile on the board")
(define TAKE   (~a "move one of the movable avatars, marked with " TURN-C " ovals, to another tile"))

(define game-frame%
  (class frame%
    (init-field board0 score0)
    
    (super-new [label TITLE] #;[stretchable-width #false] #;[stretchable-height #false])
    
    (define vp (new vertical-panel% [parent this] [border 5] [style '(border)]))
    (define tx (new text-field% [parent vp] [label #false] [enabled #f] [font menu-control-font]))
    (define hp (new horizontal-panel% [parent vp] [border 5] [style '(border)]))
    
    (define bwidth  (toint (pict-width board0)))
    (define bheight (toint (pict-height board0)))
    (define bg-snip (pict->snip board0))
    (define pboard  (new game-pasteboard% [background bg-snip] [avatars0 '[]]))
    (define editor  (new editor-canvas% [parent hp]  [min-width bwidth] [min-height bheight]
                         [style '(hide-hscroll hide-vscroll)]))
    (send editor vertical-inset 0)
    (send editor horizontal-inset 0)
    (send editor set-editor pboard)
    (define scan (new gcanvas% [parent hp] [style '(border)] [pict0 score0]))
    
    (define/public (choose board score myavatar control)
      (define (place-initial row-column gui-posn view-channel)
        (send control choose this row-column gui-posn view-channel))
      (show-state board score (place-initial-avatar board myavatar) CHOOSE #:feedback place-initial))
    
    (define/public (take-turn board score myavatars control)
      (define (move-to row-column gui-posn view-channel)
        (send control take-turn this row-column gui-posn view-channel))
      (show-state board score (place-avatars myavatars) TAKE #:feedback move-to))
    
    (define/public (show-state board score (avatars '[]) (text "") #:feedback (c void))
      (send scan set score)
      (send tx set-value text)
      (send pboard replace (pict->snip board) avatars c)
      (send editor on-paint))

    #; {Pict Pict -> [Listof [Instanceof GameImageSnip%]]}
    (define/private (place-initial-avatar board myavatar)
      (define hb (pict-height board))
      (define ha (pict-height myavatar))
      `[,(pict->snip myavatar 0 (- hb ha))])

    #; {[Listof [List Pict N N]] -> [Listof [Instanceof GameImageSnip%]]}
    (define/private (place-avatars myavatars)
      (for/list ([a myavatars])
        (match-define `(,p ,row+column [,x-ce ,y-ce]) a) ;; WARNING: matching against posn 
        (define r (posn-row row+column))
        (define c (posn-column row+column))
        (define w (toint (pict-width p)))
        (define h (toint (pict-height p)))
        (define q (cc-superimpose (filled-ellipse (- w 10) (- h 10) #:color TURN-C) p))
        (pict->snip q (- x-ce (quotient w 2)) (- y-ce (quotient h 2)) r c)))))

(define (pict->snip p [x 0] [y 0] [row #f] [column #f])
  (make-object game-image-snip% p x y row column))
