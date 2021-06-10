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
;              ;                                      ;;;                                          
;              ;                                     ;                                             
;   ; ;;;      ;      ;;;     ;;;    ;;;;            ;              ;;;;;;   ;;;;   ;    ;   ;;;;  
;   ;;  ;;     ;     ;   ;   ;   ;  ;    ;           ;;             ;  ;  ; ;;  ;;  ;;  ;;  ;    ; 
;   ;    ;     ;         ;  ;       ;;;;;;          ;  ;  ;         ;  ;  ; ;    ;   ;  ;   ;;;;;; 
;   ;    ;     ;     ;;;;;  ;       ;               ;  ;; ;         ;  ;  ; ;    ;   ;  ;   ;      
;   ;    ;     ;    ;    ;  ;       ;               ;   ; ;         ;  ;  ; ;    ;   ;;;;   ;      
;   ;;  ;;     ;    ;   ;;   ;   ;  ;;   ;          ;;   ;          ;  ;  ; ;;  ;;    ;;    ;;   ; 
;   ; ;;;       ;;;  ;;; ;    ;;;    ;;;;;           ;;;; ;         ;  ;  ;  ;;;;     ;;     ;;;;; 
;   ;                                                                                              
;   ;                                                                                              
;   ;                                                                                              
;                                                                                                  

;; use left-to-right, top-down traversal to find the first highest-value spot
(define (place-penguin s)
  (define spot* (state-board-traverse s board-lr-td (λ (x y) (list y x))))
  (when (empty? spot*)
    (error 'place-penguin "not enough spots for placing penguins"))
  (choose random-choice spot*))
                
(define (move-penguin tree)
  (define mapping (node-mapping tree))
  (cond 
    [(empty? mapping) #false]
    [(noop? tree)     (caar mapping)]
    [else
     (define steps+value
       (for/list ([1map mapping])
         (match-define (list step next) 1map)
         (list step (fish-at (fishes-board (node-current [next])) (second step)))))     
     (choose tie-breaker steps+value)]))

;; ---------------------------------------------------------------------------------------------------
#; {([Listof X] -> X) [Listof [List X Real]] -> X}
(define (choose tie-breaker x+value)
  (define the-max (max-map second x+value))
  (define others (select the-max < x+value))
  (if (empty? others) (tie-breaker (select the-max = x+value)) (random-choice others)))

#; {Real (Real Real -> Boolean) [Listof [List X Real]] -> [Listof X]}
(define (select the-max = fish-steps)
  (filter-map (λ (x) (and (= (second x) the-max) (first x))) fish-steps))
  
#; {[NEListof X] -> X}
(define (random-choice lst)
  (list-ref lst (random (length lst))))

(define (max-map f lox) (apply max (map f lox)))

;                                                                                          
;                                                                                          
;              ;                    ;                               ;                      
;     ;                             ;                               ;                      
;     ;                             ;                               ;                      
;   ;;;;;;   ;;;     ;;;;           ; ;;;    ;;;;    ;;;;     ;;;   ;   ;    ;;;;    ;;;;  
;     ;        ;    ;    ;          ;;  ;;   ;;  ;  ;    ;   ;   ;  ;  ;    ;    ;   ;;  ; 
;     ;        ;    ;;;;;;          ;    ;   ;      ;;;;;;       ;  ;;;     ;;;;;;   ;     
;     ;        ;    ;               ;    ;   ;      ;        ;;;;;  ;;;     ;        ;     
;     ;        ;    ;               ;    ;   ;      ;       ;    ;  ;  ;    ;        ;     
;     ;        ;    ;;   ;          ;;  ;;   ;      ;;   ;  ;   ;;  ;   ;   ;;   ;   ;     
;      ;;;   ;;;;;   ;;;;;          ; ;;;    ;       ;;;;;   ;;; ;  ;    ;   ;;;;;   ;     
;                                                                                          
;                                                                                          
;                                                                                          
;                                                                                          

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

