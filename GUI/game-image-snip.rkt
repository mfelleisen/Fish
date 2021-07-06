#lang racket/gui

;; enahnace the image snip with some fields that connect it back to the model

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
 game-image-snip%)

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
(require pict)

;                                  
;                                  
;                      ;           
;                                  
;                                  
;    ;;;;   ; ;;;    ;;;    ; ;;;  
;   ;    ;  ;;   ;     ;    ;;  ;; 
;   ;       ;    ;     ;    ;    ; 
;    ;;;;   ;    ;     ;    ;    ; 
;        ;  ;    ;     ;    ;    ; 
;   ;    ;  ;    ;     ;    ;;  ;; 
;    ;;;;   ;    ;   ;;;;;  ; ;;;  
;                           ;      
;                           ;      
;                           ;      
;                                  

(define game-image-snip%
  (class image-snip%
    (init-field p x0-tl y0-tl row0 column0)
    ;; the pict, the graphical top-left position, the logical row-column place 
    
    (field [width  (toint (pict-width p))])
    (field [height (toint (pict-height p))])

    (super-make-object (pict->bitmap p))
    
    (define/public (get-x0) x0-tl)
    (define/public (get-y0) y0-tl)

    (define/public (translate x y #:place t)
      (case t
        [(top->center) (values (+ x (quotient width 2)) (+ y (quotient height 2)))]
        [(center->top) (values (- x (quotient width 2)) (- y (quotient height 2)))]))

    (define/public (current-row-column)
      (if (number? row0) (list row0 column0) #f))

    (define/public (scale2 w-factor h-factor)
      (define new-p (scale p w-factor h-factor))
      (define new-x (toint (* w-factor x0-tl)))
      (define new-y (toint (* h-factor y0-tl)))
      (new game-image-snip% [p new-p] [x0-tl new-x] [y0-tl new-y] [row0 row0] [column0 column0]))))
