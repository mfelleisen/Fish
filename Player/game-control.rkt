#lang racket/gui

;; the actual control classes for a human player mechanism

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

(require (only-in Fish/Player/game-base-control control/c controller/c))

(define initial/c (and/c (class/c [choose controller/c]) control/c))

(define turn/c (and/c (class/c [take-turn controller/c]) control/c))

(provide
 (contract-out
  [initial/c contract?]
  [turn/c    contract?]

  ;; a class for controlling initial placement choices
  [initial-control% initial/c]
  
  ;; a class for controlling turns
  (tt-control% turn/c)))

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

(require (except-in Fish/Player/game-base-control control/c controller/c))

;                                                          
;                                                          
;                                                    ;;;   
;                             ;                        ;   
;                             ;                        ;   
;     ;;;    ;;;;   ; ;;;   ;;;;;;   ;;;;    ;;;;      ;   
;    ;   ;  ;;  ;;  ;;   ;    ;      ;;  ;  ;;  ;;     ;   
;   ;       ;    ;  ;    ;    ;      ;      ;    ;     ;   
;   ;       ;    ;  ;    ;    ;      ;      ;    ;     ;   
;   ;       ;    ;  ;    ;    ;      ;      ;    ;     ;   
;    ;   ;  ;;  ;;  ;    ;    ;      ;      ;;  ;;     ;   
;     ;;;    ;;;;   ;    ;     ;;;   ;       ;;;;       ;;;
;                                                          
;                                                          
;                                                          
;                                                          

(define INITIAL-TITLE "placing an avatar")
(define TURN-TITLE "moving an avatar")

(define initial-control%
  (make-control% 
   (class base-control% (super-new)
     
     (field [TITLE    INITIAL-TITLE]
            [2model   (λ (from to) to)]
            [ok-step? (λ (from to) #true)])

     (define/public (choose . x) (send this control . x)))))

(define tt-control%
  (make-control%
   (class base-control% (super-new)
     
     (init-field    ok-step?)
     (field [TITLE  TURN-TITLE]
            [2model list])
     
     (define/public (take-turn . x) (send this control . x)))))
