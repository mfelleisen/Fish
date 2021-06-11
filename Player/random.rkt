#lang racket

;; implement a random-pick strategy that avoids the "maximal fish tile" when possible
;; (This makes the strategy inferior to 'greedy` and thus hopefully testable.) 

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

(provide random-strategy)

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

(require Fish/Player/strategy-interface)
(require (except-in Fish/Common/rules tree?))
(require (except-in Fish/Common/game-state fishes? turn? move/c))
(require Fish/Common/board)

(module+ test
  (require (submod Fish/Player/greedy examples))
  (require rackunit))

;                                            
;                            ;               
;                            ;               
;                            ;               
;    ;;;;  ;;;;   ; ;;    ;;;;   ;;;  ;;;;;; 
;    ;;  ;     ;  ;;  ;  ;; ;;  ;; ;; ;  ;  ;
;    ;         ;  ;   ;  ;   ;  ;   ; ;  ;  ;
;    ;      ;;;;  ;   ;  ;   ;  ;   ; ;  ;  ;
;    ;     ;   ;  ;   ;  ;   ;  ;   ; ;  ;  ;
;    ;     ;   ;  ;   ;  ;; ;;  ;; ;; ;  ;  ;
;    ;      ;;;;  ;   ;   ;;;;   ;;;  ;  ;  ;
;                                            
;                                            
;                                            
                                    
(define random-strategy
  (class base-strategy%
    (super-new)

    (define/override (evaluate trn state)
      (fish-at (fishes-board (tree-current state)) (second trn)))

    (define/augment (choose the-max x+value)
      (select the-max < x+value))))


;                                                                                  
;                                                                                  
;                                                                                  
;                             ;;;             ;                       ;            
;                            ;                ;                       ;            
;    ;;;;   ;;  ;;           ;              ;;;;;;   ;;;;    ;;;;   ;;;;;;   ;;;;  
;   ;    ;   ;  ;            ;;               ;     ;    ;  ;    ;    ;     ;    ; 
;   ;;;;;;    ;;            ;  ;  ;           ;     ;;;;;;  ;         ;     ;      
;   ;         ;;            ;  ;; ;           ;     ;        ;;;;     ;      ;;;;  
;   ;         ;;            ;   ; ;           ;     ;            ;    ;          ; 
;   ;;   ;   ;  ;           ;;   ;            ;     ;;   ;  ;    ;    ;     ;    ; 
;    ;;;;;  ;    ;           ;;;; ;            ;;;   ;;;;;   ;;;;      ;;;   ;;;;  
;                                                                                  
;                                                                                  
;                                                                                  
;                                                                                  

(define g (new random-strategy))
(define (place-penguin s) (send g place-penguin s))
(define (move-penguin t) (send g move-penguin t))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (define short-row
    (let* ([s (create-state 2 2 '(a b) #:holes '[[0 1] [1 1]])]
           [s (place-avatar s '[0 0])])
      s))
  (define no-spots (place-avatar short-row '[1 0]))
  (check-exn #px"not enough" (λ () (place-penguin no-spots)) "bug 3, gen rec out of spots")
  (check-true   (posn/c (place-penguin 2-state-no-action-2)) "origin")
  (check-true   (posn/c (place-penguin 2-state-1-action-7)) "one over")
  (check-equal? (place-penguin special) '[1 0] "one down")
  (check-equal? (place-penguin 2-state-0-action-1player2) '[1 1])
  (check-equal? (place-penguin ran-placement-state) '[1 1] "only one non-maximal tile left"))

(module+ test
  (check-equal? (move-penguin (generate-tree 2-state-no-action-2)) A2-state-no-action-2 "final")
  (check-equal? (move-penguin tree-2-state-1-action-7) A2-state-1-action-7)
  (check-true   (skip? (move-penguin tree-2-state-0-action-1player2)) "cant")
  (check-equal? (move-penguin tree-2-state-1action-first-player) A2-state-1action-first-player)
  (check-equal? (move-penguin tree-3-state-2) A3-state-2)
  (check-equal? (move-penguin tree-ran-move) '[[0 1] [1 1]] "go below greedy step"))

