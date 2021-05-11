#lang racket

;; print the history of how we got to an error in a trace contract

;                                                                          
;                                                                          
;                                                            ;;;           
;                     ;                                        ;           
;                     ;                                        ;           
;    ;;;;   ;;  ;;  ;;;;;;   ;;;;    ;;;;   ; ;;;     ;;;      ;     ;;;;  
;   ;    ;   ;  ;     ;     ;    ;   ;;  ;  ;;   ;   ;   ;     ;    ;    ; 
;   ;;;;;;    ;;      ;     ;;;;;;   ;      ;    ;       ;     ;    ;      
;   ;         ;;      ;     ;        ;      ;    ;   ;;;;;     ;     ;;;;  
;   ;         ;;      ;     ;        ;      ;    ;  ;    ;     ;         ; 
;   ;;   ;   ;  ;     ;     ;;   ;   ;      ;    ;  ;   ;;     ;    ;    ; 
;    ;;;;;  ;    ;     ;;;   ;;;;;   ;      ;    ;   ;;; ;      ;;;  ;;;;  
;                                                                          
;                                                                          
;                                                                          
;                                                                          

(provide
 #; {Boolean [Listof XPlayer] [Listof [Listof XPlayer]] [Listof [Listof TurnAction]] [Listof State]
             -> Boolean}
 verbose-error-report

 #; {Boolean Parameter}
 error-printing?)

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

(require Fish/Common/game-state)
(require Fish/Lib/xsend)

;                                          
;                                          
;                      ;                   
;                                     ;    
;                                     ;    
;   ; ;;;    ;;;;    ;;;    ; ;;;   ;;;;;; 
;   ;;  ;;   ;;  ;     ;    ;;   ;    ;    
;   ;    ;   ;         ;    ;    ;    ;    
;   ;    ;   ;         ;    ;    ;    ;    
;   ;    ;   ;         ;    ;    ;    ;    
;   ;;  ;;   ;         ;    ;    ;    ;    
;   ; ;;;    ;       ;;;;;  ;    ;     ;;; 
;   ;                                      
;   ;                                      
;   ;                                      
;                                          

(define error-printing? (make-parameter #false))
(define error-printing-channel #false)

(define (verbose-error-report okay? player-order* this* state* actions*)
  (unless okay?
    (when (error-printing?)
      (unless error-printing-channel
        (set! error-printing-channel (error-printing-thread format-state-sequence)))
      (define error-info
        (for/list ([t this*] [p player-order*] [a actions*] [s state*])
          (list t p a s)))
      (channel-put error-printing-channel error-info)))
  okay?)

#; {[Listof [List XPlayer [Listof XPlayer] [Listof TurnAction] State]] -> Void}
(define (format-state-sequence state)
  (define (ID p) (get-field me p))
  (parameterize ([current-output-port (current-error-port)])
    (displayln "players called out of order; how we got here:")
    (for ([x (reverse state)])
      (match-define `[,aplayer ,order ,a* ,state] x)
      (let-values ([(x _) (render-state state)])
        (pretty-print `[,(ID aplayer) ,(map ID order) ,a* ,x])))))