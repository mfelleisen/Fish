#lang racket

;; a trace contract that checks the "referee promise" of calling all players
;; in the order in which they got handed to the referee, except in "skip states"

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
 #; {(U State [Listof XPlayer] XPlayer [Listof Actions]) #:name Symbol -> [U False Accu]}
 call-players-in-given-order-unless-skipped)

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

(require Fish/Common/player-interface-error-reporting)
(require Fish/Common/game-state)
(require Fish/Common/internal-player)
(require Fish/Lib/list)
(require Fish/Lib/struct-accessor)
(require (for-syntax syntax/parse))

;                                                          
;                                                          
;                                      ;                   
;     ;                                                    
;     ;                                                    
;   ;;;;;;   ;;;;     ;;;     ;;;    ;;;    ; ;;;    ;;; ; 
;     ;      ;;  ;   ;   ;   ;   ;     ;    ;;   ;  ;;  ;; 
;     ;      ;           ;  ;          ;    ;    ;  ;    ; 
;     ;      ;       ;;;;;  ;          ;    ;    ;  ;    ; 
;     ;      ;      ;    ;  ;          ;    ;    ;  ;    ; 
;     ;      ;      ;   ;;   ;   ;     ;    ;    ;  ;;  ;; 
;      ;;;   ;       ;;; ;    ;;;    ;;;;;  ;    ;   ;;; ; 
;                                                        ; 
;                                                    ;  ;; 
;                                                     ;;;  
;                                                          

(struct accu [order this states actions] #:prefab)
#; {Accu = [accu [Listof State] [Listof XPlayer] [Listof [Listof XPlayer]] [Listof [Listof Actions]]]}

(define (call-players-in-given-order-unless-skipped x [aa (accu '() '() '() '())] #:name which-trace)
  (case which-trace
    [(player*/c) (accu (list x) '() '() '[])]
    [(this/c)    (accu+ this)]
    [(state/c)   (accu+ states)]
    [(actions/c) (let-values ([(aa OK) (okay? (accu+ actions))]) (and OK (rotate-player-order aa)))]))

#; {Accu -> Accu}
(define (rotate-player-order aa)
  (match-define (accu (cons order x) aplayer state* actions*) aa)
  (accu (cons (rotate order) x) aplayer state* actions*))

#; {Accu -> Boolean}
;; EFFECT modify the player-order in `aa` to accommodate skips and drop-out players 
(define (okay? aa)
  (match-define (accu (cons O _) (and this* (cons T _1)) (and state* (cons S _2)) (and actions* (cons A _3))) aa)
  (let ([player-order (revised-order O A)])
    (define current-player* (fishes-players S))
    (let*-values ([(expected-player player-order)
                   (if (= (length current-player*) (length player-order))
                       (values (first player-order) player-order)
                       ;; a player dropped:
                       (values T (map iplayer-payload current-player*)))]
                  [(okay?) (equal? expected-player T)]
                  [(aa) (accu+ order player-order)])
      (verbose-error-report okay? (accu-order aa) this* state* actions*)
      (values aa okay?))))

#; {[Listof X] [Listof (U Y Symbol)] -> [Listof X]}
;; push the Xs to the end of `player-order` while there are SKIPs at the end of actions0
(define (revised-order player-order0 actions0)
  (let loop ([actions (reverse actions0)] [order player-order0])
    (cond
      [(empty? actions) order]
      [else (if (skip? (first actions)) (loop (rest actions) (rotate order)) order)])))

;; ---------------------------------------------------------------------------------------------------
;; SYNTAX: totally specialized to the above code 
;; ASSUME `aa` is defined in the lexical scope of `field`
;; ASSUME all fields of an accu structure are lists 
#; (accu+ field y)
;; cons `y` -- must be defined in the scope of `field` -- to the value of `field` in `aa`
;;  .. leaves all other fields alone
#; (accu+ field)
;; uses the identifier #'x for `y` 

(define-syntax (accu+ stx)
  (syntax-parse stx
    [(_ field)
     #:with x (datum->syntax #'field 'x)
     #'(accu+ field x)]
    [(_ field x)
     #:with a (datum->syntax #'field 'aa)
     #'(struct-copy accu a [field (cons x (struct-accessor accu field a))])]))
