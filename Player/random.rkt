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

(require (only-in Fish/Common/game-tree tree?))
(require (only-in Fish/Common/game-state fishes? turn? move/c))
(require (only-in Fish/Common/board posn/c))

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
(require (except-in Fish/Common/game-tree tree?))
(require (except-in Fish/Common/game-state fishes? turn? move/c))
(require (except-in Fish/Common/board posn/c))

(module+ test
  (require (submod Fish/Player/greedy examples))
  (require rackunit))

;                                                                                  
;                                                                                  
;            ;;;                                                                   
;              ;                                                      ;            
;              ;                                                      ;            
;   ; ;;;      ;      ;;;     ;;;    ;;;;   ;;;;;;   ;;;;   ; ;;;   ;;;;;;   ;;;;  
;   ;;  ;;     ;     ;   ;   ;   ;  ;    ;  ;  ;  ; ;    ;  ;;   ;    ;     ;    ; 
;   ;    ;     ;         ;  ;       ;;;;;;  ;  ;  ; ;;;;;;  ;    ;    ;     ;      
;   ;    ;     ;     ;;;;;  ;       ;       ;  ;  ; ;       ;    ;    ;      ;;;;  
;   ;    ;     ;    ;    ;  ;       ;       ;  ;  ; ;       ;    ;    ;          ; 
;   ;;  ;;     ;    ;   ;;   ;   ;  ;;   ;  ;  ;  ; ;;   ;  ;    ;    ;     ;    ; 
;   ; ;;;       ;;;  ;;; ;    ;;;    ;;;;;  ;  ;  ;  ;;;;;  ;    ;     ;;;   ;;;;  
;   ;                                                                              
;   ;                                                                              
;   ;                                                                              
;                                                                                  

;; use left-to-right, top-down traversal to find the first highest-value spot
(define (place-penguin s)
  (define spot* (state-board-traverse s board-lr-td cons))
  (cond
    [(empty? spot*) (error 'place-penguin "not enough spots for placing penguins")]
    [else
     (define max (argmax car spot*))
     (define others (filter-map (λ (x) (and (not (= (car max) (car x))) (cdr x))) spot*))
     (if (empty? others) (cdr max) (random-choice others))]))
  
(module+ test 
  (check-equal? (place-penguin (create-state 2 2 '(a b) #:fixed 2 #:holes '[[0 0]])) '[0 0] "place 1")
  
  (define short-row
    (let* ([s (create-state 2 2 '(a b) #:holes '[[0 1] [1 1]])]
           [s (place-avatar s '[0 0])])
      s))
  (check-equal? (place-penguin short-row) '[1 0] "place 2")

  (define no-spots (place-avatar short-row '[1 0]))
  (check-exn #px"not enough" (λ () (place-penguin no-spots)) "bug 3, gen rec out of spots"))
    
;                                                                          
;                                                                          
;                   ;                                                      
;     ;             ;                         ;                            
;     ;             ;                         ;                            
;   ;;;;;;    ;;;   ;   ;    ;;;;           ;;;;;;  ;    ;   ;;;;   ; ;;;  
;     ;      ;   ;  ;  ;    ;    ;            ;     ;    ;   ;;  ;  ;;   ; 
;     ;          ;  ;;;     ;;;;;;            ;     ;    ;   ;      ;    ; 
;     ;      ;;;;;  ;;;     ;                 ;     ;    ;   ;      ;    ; 
;     ;     ;    ;  ;  ;    ;                 ;     ;    ;   ;      ;    ; 
;     ;     ;   ;;  ;   ;   ;;   ;            ;     ;   ;;   ;      ;    ; 
;      ;;;   ;;; ;  ;    ;   ;;;;;             ;;;   ;;; ;   ;      ;    ; 
;                                                                          
;                                                                          
;                                                                          
;                                                                          

(define (move-penguin tree)
  (define state   (node-current tree))
  (define players (fishes-players state))
  (define cplayer (fishes-current-player state))
  (define mapping (node-mapping tree))
  (cond 
    [(empty? mapping) #false]
    [(noop? tree)    (caar mapping)]
    [else (maximal-fish-step cplayer mapping)]))

(define (maximal-fish-step cplayer mapping)
  (all-max
   (for/list ([1map mapping])
     (define next [(second 1map)])
     (define step (first 1map))
     (list step (fish-at (fishes-board (node-current next)) (second step))))))

(define (all-max fish-steps)
  (define the-max (second (argmax second fish-steps)))
  (define others (filter-map (λ (x) (and (< (second x) the-max) (first x))) fish-steps))
  (if (empty? others)
      (tie-breaker (filter-map (λ (x) (and (= (second x) the-max) (first x))) fish-steps))
      (random-choice others)))

;; ---------------------------------------------------------------------------------------------------
#; {[NEListof X] -> X}
(define (random-choice lst)
  (list-ref lst (random (length lst))))

;; ---------------------------------------------------------------------------------------------------
#; (-> (and/c (listof move/c) cons?) move/c)
;; implemen a tie breaker if there are serveal equally valued player actions:
;; in order, apply the following "filters" to reduce the list:
;; top-most row of `from` field, `left-most` column of `from`, top-most for `to`, left-most for to
(define (tie-breaker lop)
  (define-syntax whittle-down-to-1
    (syntax-rules ()
      [(_ x)
       (cond [(empty? (rest x)) (first x)] [else (error 'tie-breaker "catastrophe!! ~a" x)])]
      [(_ x f g ...)
       (cond [(empty? (rest x)) (first x)] [else (define y (f x)) (whittle-down-to-1 y g ...)])]))
  
  (whittle-down-to-1
   lop
   (closest-to-origin-by posn-row first)
   (closest-to-origin-by posn-column first)
   (closest-to-origin-by posn-row first flip-from-and-to)
   (closest-to-origin-by posn-column first flip-from-and-to)))
                                          
#; { [Lisy Posn Posn] -> [List Posn Posn] }
(define flip-from-and-to [match-lambda [[list from to] [list to from]]])

#; { {Listof (->) [List Posn Posn]} -> [List Posn Posn] }
(define ((closest-to-origin-by . sel*) lop)
  (define sel (apply compose sel*))
  (define sorted-by-row (sort lop < #:key sel)) 
  (define lowest-row    (sel (first sorted-by-row)))
  (define all-lowest    (takef sorted-by-row (λ (x) (= (sel x) lowest-row))))
  all-lowest)
  
;; ---------------------------------------------------------------------------------------------------
(module+ test
  (define just-0-0 '[[0 1][0 3]])
  (check-equal? (tie-breaker `{,just-0-0}) just-0-0 "a")
  (check-equal? (tie-breaker `{ [[0 1] [0 8]] [[0 1] [0 3]] [[1 0] [2 0]] }) just-0-0 "b")
  (check-equal? (tie-breaker `{ [[0 1] [0 8]] [[0 1] [0 3]] }) just-0-0 "c")
  (check-equal? (tie-breaker '{ [[0 1] [2 0]] [[0 1] [3 0]] }) '[ [0 1] [2 0] ] "BUG")

  (check-exn #px"catastrophe" (λ () (tie-breaker `{ [[0 1] [0 3]] [[0 1] [0 3]] }))))

;; ---------------------------------------------------------------------------------------------------

(define random-strategy (strategy place-penguin move-penguin))

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

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (check-equal? (place-penguin 2-state-no-action-2) '[0 0] "origin")
  (check-equal? (place-penguin 2-state-1-action-7) '[0 1] "one over")
  (check-equal? (place-penguin special) '[1 0] "one down")
  (check-equal? (place-penguin 2-state-0-action-1player2) '[1 1])
  (check-equal? (place-penguin ran-placement-state) '[1 1] "only one non-maximal tile left"))

(module+ test
  (check-equal? (move-penguin (generate-tree 2-state-no-action-2)) A2-state-no-action-2 "final")
  (check-equal? (move-penguin tree-2-state-1-action-7) A2-state-1-action-7)
  (check-true   (skip? (move-penguin tree-2-state-0-action-1player2)) "cant")
  (check-equal? (move-penguin tree-2-state-1action-first-player) A2-state-1action-first-player)
  (check-equal? (move-penguin tree-3-state-2) A3-state-2)
  (check-equal? (move-penguin ran-move-tree) '[[0 1] [1 1]] "go below greedy step"))

