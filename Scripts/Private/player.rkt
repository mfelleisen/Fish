#lang racket

;; this component implements the mechanics of a logging player

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

(provide (contract-out [player% player%/c]))
  

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


(require (prefix-in base: Fish/Player/player))

;                              
;   ;                          
;   ;                          
;   ;                          
;   ;;;;   ;;;;    ;;;    ;;;  
;   ;; ;;      ;  ;   ;  ;;  ; 
;   ;   ;      ;  ;      ;   ;;
;   ;   ;   ;;;;   ;;;   ;;;;;;
;   ;   ;  ;   ;      ;  ;     
;   ;; ;;  ;   ;  ;   ;  ;     
;   ;;;;    ;;;;   ;;;    ;;;; 
;                              
;                              
;

(define player%
  (class base:player%
    (define-syntax-rule
      (define/logging (head x ...))
      (define/override (head x ...)
        (displayln `[the remote player was called with (head ,x ...)])
        (super head x ...)))

    [define/logging (playing-as my-name)]
    [define/logging (playing-with others)]
    (define/logging (initial state))
    [define/logging (take-turn state actions-since-last-turn)]
    (define/logging (start-of-tournament nicknames))
    [define/override (end-of-tournament result)
      (displayln `[the logging player ,(if result "won" "lost")])]
      
    
    (super-new)))

