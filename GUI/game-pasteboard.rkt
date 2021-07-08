#lang racket/gui

;; a pasteboard editor for displaying the current state of the board and movable avatar snips

;; it 'knows' nothing about the meaning of the snips or the background, though it makes one
;; assumption about the size of the snip it gets

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

(require (only-in Fish/GUI/game-image-snip game-image-snip%))
(require (only-in Fish/Common/board posn/c))

(provide
 (contract-out
  [game-pasteboard%
   (class/c
    [init-field
     [background (is-a?/c game-image-snip%)]
     [avatars0   (listof (is-a?/c game-image-snip%))]
     [control    [-> (is-a?/c game-image-snip%) posn/c [channel/c any/c] any]]])]))

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

(require Fish/Lib/toint)

;                                                                                  
;                                                                                  
;                                           ;                                    ; 
;                             ;             ;                                    ; 
;                             ;             ;                                    ; 
;   ; ;;;     ;;;    ;;;;   ;;;;;;   ;;;;   ; ;;;    ;;;;     ;;;    ;;;;    ;;; ; 
;   ;;  ;;   ;   ;  ;    ;    ;     ;    ;  ;;  ;;  ;;  ;;   ;   ;   ;;  ;  ;;  ;; 
;   ;    ;       ;  ;         ;     ;;;;;;  ;    ;  ;    ;       ;   ;      ;    ; 
;   ;    ;   ;;;;;   ;;;;     ;     ;       ;    ;  ;    ;   ;;;;;   ;      ;    ; 
;   ;    ;  ;    ;       ;    ;     ;       ;    ;  ;    ;  ;    ;   ;      ;    ; 
;   ;;  ;;  ;   ;;  ;    ;    ;     ;;   ;  ;;  ;;  ;;  ;;  ;   ;;   ;      ;;  ;; 
;   ; ;;;    ;;; ;   ;;;;      ;;;   ;;;;;  ; ;;;    ;;;;    ;;; ;   ;       ;;; ; 
;   ;                                                                              
;   ;                                                                              
;   ;                                                                              
;                                                                                  

(define game-pasteboard%
  (class pasteboard%
    (init-field background avatars0 (control void))
    
    (inherit move-to insert resize get-snip-location is-selected?)
    (inherit begin-edit-sequence end-edit-sequence delete get-view-size)

    (field [width0  (get-field width background)])  ;; of the original display 
    (field [height0 (get-field height background)]) ;; ditto
    
    ;; -----------------------------------------------------------------------------------------------
    ;; size managamennt
    ;; ASSUME all background picts are of the same size 

    (field [the-width  width0])  ;; of the current display 
    (field [the-height height0]) ;; ditto

    (field [scaled-width  1.0]) ;; the scaled width from the very first background 
    (field [scaled-height 1.0]) ;; ditto

    (define/private (inverse-scaled x-ce y-ce)
      (list (toint (/ x-ce scaled-width)) (toint (/ y-ce scaled-height))))

    (define/private (scaled x-ce y-ce)
      (values (toint (* x-ce scaled-width)) (toint (* y-ce scaled-height))))

    (define/augment (on-display-size)
      (define-values (scaled-width scaled-height) (scaling-ratios))
      (define &background (send background scale2 scaled-width scaled-height))
      (define &avatars0   (map (λ (a) (send a scale2 scaled-width scaled-height)) avatars0))
      (do-replace &background &avatars0))
    
    (define/public (replace &background &avatars0 &control)
      (define bg (send &background scale2 scaled-width scaled-height))
      (define av (map (λ (a) (send a scale2 scaled-width scaled-height)) &avatars0))
      (do-replace bg av)
      (set! control &control))

    (define/private (do-replace &background &avatars0)
      (set! the-selected-snip #false)
      (begin-edit-sequence)
      (for ([s avatars0]) (delete s))
      (delete background)
      (set! background &background)
      (set! avatars0   &avatars0)
      (show)
      (end-edit-sequence))

    #; {-> (values Real Real)}
    ;; EFFECT set width, height, scaled-width, and scaled-height
    (define/private (scaling-ratios)
      (define-values (width height) (my-dimensions))
      (begin0
        (values (/ width the-width) (/ height the-height))
        (set! scaled-width  (/ width width0))
        (set! scaled-height (/ height height0))
        (set! the-width     width)
        (set! the-height    height)))

    #; { -> (values N N)}
    ;; determine the current size of the pasteboard
    (define/private (my-dimensions)
      (define bw (box 0))
      (define bh (box 0))
      (get-view-size bw bh)
      (values (unbox bw) (unbox bh)))

    ;; now show initial state 
    (define/private (show)
      (for ([snip avatars0])
        (insert snip (send snip get-x0) (send snip get-y0)))
      (insert background (send background get-x0) (send background get-y0)))

    ;; -----------------------------------------------------------------------------------------------
    ;; snip move management 

    (field [the-selected-snip #false])

    (define/augment (can-select? s on?)
      (cond
        [(not on?) (set! the-selected-snip #false) #true]
        [else (and (not the-selected-snip) (not (eq? s background)))]))

    (define/augment (after-select the-snip on?)
      (set! the-selected-snip (and on? the-snip)))

    (define/augment (can-interactive-move? e)
      (not (is-selected? background)))
    
    (define/augment (after-interactive-move event)
      (when the-selected-snip
        (define-values (x-tl y-tl) (find-snip-location the-selected-snip))
        (define-values (x-ce y-ce) (send the-selected-snip translate x-tl y-tl #:place 'top->center))
        (wait-for-next-location the-selected-snip x-ce y-ce)))
    
    #; {[Instanceof Snip%] N N -> Void}
    ;; communicate with the concurrent control component to get the next location of the snip
    (define/private (wait-for-next-location the-snip x-ce y-ce)
      (define view-channel (make-channel))
      (control (send the-snip current-row-column) (inverse-scaled x-ce y-ce) view-channel)
      (define move-snip-to (channel-get view-channel))
      (match move-snip-to
        [#false
         (move-to the-snip (send the-snip get-x0) (send the-snip get-y0))]
        [[list cx0 cy0]
         (define-values (cx cy) (scaled cx0 cy0))
         (define-values (x-tl y-tl) (send the-snip translate cx cy #:place 'center->top))
         (move-to the-snip x-tl y-tl)]))

    #; {[Instanceof Snip] Boolean -> N N}
    ;; retrireveretrieve the top-left corner of the snip
    (define/private (find-snip-location the-snip)
      (define x-tl (box 10))
      (define y-tl (box 10))
      (get-snip-location the-snip x-tl y-tl)
      (define x (toint (unbox x-tl)))
      (define y (toint (unbox y-tl)))
      (values x y))

    ;; -----------------------------------------------------------------------------------------------
    ;; go
    
    (super-new)
    (show)))
