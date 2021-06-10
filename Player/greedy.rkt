#lang racket

;; implement a simple greedy strategy:
;; -- place penguins on a tile with maximal fish number 
;; -- move he penguin to a tile with maximal fish number
;;;   using the `tiebreaker` if there are several 


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

(require (only-in Fish/Common/rules tree?))
(require (only-in Fish/Common/game-state fishes? turn? move/c))
(require (only-in Fish/Common/board posn/c))

(provide greedy-strategy)

(module+ examples
  (provide
   1row 1column
   2-state-no-action-2 2-state-1-action-7 2-state-0-action-1player2 2-state-2 3-state-2
   A2-state-no-action-2 A2-state-1-action-7 A2-state-2 A3-state-2
   B2-state-no-action-2
   ran-placement-state
   tree-ran-move))

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
(require (except-in Fish/Common/board posn/c))

(module+ examples
  (require (for-syntax syntax/parse))
  (require (for-syntax racket/syntax)))

(module+ test
  (require (submod ".." examples))
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
  (choose first spot*))

(define (move-penguin tree)
  (define mapping (node-mapping tree))
  (cond 
    [(empty? mapping) #false]
    [(noop? tree)    (caar mapping)]
    [else
     (define steps+value
       (for/list ([1map mapping])
         (match-define (list step next) 1map)
         (list step (fish-at (fishes-board (node-current [next])) (second step)))))
     (choose tie-breaker steps+value)]))

#; {([Listof X] -> X) [Listof [List X Real]] -> X}
(define (choose tie-breaker xvalue)
  (define the-max (max-map second xvalue))
  (define all-max (filter-map (λ (x) (and (= (second x) the-max) (first x))) xvalue))
  (tie-breaker all-max))

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

(define greedy-strategy (strategy place-penguin move-penguin))

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

(module+ examples
  #; (define-stntax name ...)
  ;; generates a state `name` and its tree named `tree-name` and its picture named `pict-name`
  (define-syntax (define-state stx)
    (syntax-parse stx
      [(define-state name e0 [(name1 ...) e] ...)
       #:with tree-name (format-id #'name "tree-~a" (syntax-e #'name))
       #:with pict-name (format-id #'name "pict-~a" (syntax-e #'name))
       #'(define-values (name pict-name tree-name)
           (let*-values ([(name) e0]
                         [(name1 ... name) e] ...
                         [(pict _) (render-state name)])
             (values name pict (generate-tree name))))]))
  
  (define-state 2-state-no-action-2 (create-state 2 3 '(x y z) #:fixed 1))
  (define A2-state-no-action-2 #false)
  (define B2-state-no-action-2 '[0 0])
  (define-state 2-state-1-action-7 (create-state 2 3 '(x y z) #:fixed 1)
    [() (place-avatar 2-state-1-action-7 '[0 0])])
  (define A2-state-1-action-7 '((0 0) (1 0)))
  (define-state 2-state-0-action-1player2
    (create-state 2 2 '(x y) #:fixed 2)
    [() (create-state 2 2 '(x y) #:fixed 2)]
    [()  (place-avatar 2-state-0-action-1player2 '[0 0])]
    [() (next-player (place-avatar 2-state-0-action-1player2 '[1 0]))]
    [() (next-player (place-avatar 2-state-0-action-1player2 '[0 1]))])
  (define-state 2-state-1action-first-player 
    (create-state 2 2 '(x y) #:fixed 2)
    [() (place-avatar 2-state-1action-first-player '[0 0])]
    [() (next-player (place-avatar 2-state-1action-first-player '[1 0]))]
    [() (place-avatar 2-state-1action-first-player '[0 1])])
  (define A2-state-1action-first-player '[[0 1] [1 1]])
  (define-state 2-state-2 (create-state 2 3 '(x y) #:fixed 2)
    [() (next-player (place-avatar 2-state-2 '[1 0]))]
    [() (place-avatar 2-state-2 '[0 1])]
    [() (next-player (place-avatar 2-state-2 '[1 1]))])
  (define A2-state-2 '[[1 0] [0 0]])
  (define-state 3-state-2 (create-state 2 3 '(x y) #:holes '{[1 2]})
    [() (next-player (place-avatar 3-state-2 '[1 0]))]
    [() (place-avatar 3-state-2 '[0 1])]
    [() (next-player (place-avatar 3-state-2 '[1 1]))])
  (define A3-state-2 '[[1 0] [0 0]])
  (define-state special (create-state 2 1 '(x y))
    [() (place-avatar special '[0 0])])

  (define-state 1column (create-state 1 1 '(x y z w)))
  
  (define-state 1row (create-state 1 25 '(x y))
    [() (next-player (place-avatar 1row '[0 0]))]
    [() (next-player (place-avatar 1row '[0 1]))]
    [() (next-player (place-avatar 1row '[0 2]))]
    [() (next-player (place-avatar 1row '[0 3]))]
    [() (next-player (place-avatar 1row '[0 4]))]
    [() (next-player (place-avatar 1row '[0 5]))]
    [() (next-player (place-avatar 1row '[0 6]))]
    [() (next-player (place-avatar 1row '[0 7]))])

  (provide ;; additional provides for testing 
   special
   tree-2-state-2 tree-3-state-2
   tree-2-state-1action-first-player
   A2-state-1action-first-player
   tree-2-state-0-action-1player2
   tree-2-state-1-action-7))

(module+ examples

  (require (submod Fish/Common/game-state serialize))

  (define ran-placement-state
    (state
     (make-hasheq `((board . ((2 3) (2 1) (5 5)))
                    (players . ,(list
                                 #hasheq((color . "white") (places . ((0 1))) (score . 0))
                                 #hasheq((color . "red") (places . ((1 0) (0 0))) (score . 0))))))
     #:soft #true))

  (define-state ran-move (place-avatar ran-placement-state (place-penguin ran-placement-state))))

;; ---------------------------------------------------------------------------------------------------
(module+ test 
  (check-equal? (place-penguin (create-state 2 2 '(a b) #:fixed 2 #:holes '[[0 0]])) '[0 0] "place 1")
  
  (define short-row
    (let* ([s (create-state 2 2 '(a b) #:holes '[[0 1] [1 1]])]
           [s (place-avatar s '[0 0])])
      s))
  (check-equal? (place-penguin short-row) '[1 0] "place 2")

  (define no-spots (place-avatar short-row '[1 0]))
  (check-exn #px"not enough" (λ () (place-penguin no-spots)) "bug 3, gen rec out of spots"))

(module+ test
  (check-equal? (place-penguin 2-state-no-action-2) '[0 0] "origin")
  (check-equal? (place-penguin 2-state-1-action-7) '[0 1] "one over")
  (check-equal? (place-penguin special) '[1 0] "one down")
  (check-equal? (place-penguin 2-state-0-action-1player2) '[1 1])
  (check-equal? (place-penguin ran-placement-state) '[2 0] "first argmax of [2 0], [2 2]"))

(module+ test
  (check-equal? (move-penguin (generate-tree 2-state-no-action-2)) A2-state-no-action-2 "final")
  (check-equal? (move-penguin tree-2-state-1-action-7) A2-state-1-action-7)
  (check-true   (skip? (move-penguin tree-2-state-0-action-1player2)) "cant")
  (check-equal? (move-penguin tree-2-state-1action-first-player) A2-state-1action-first-player)
  (check-equal? (move-penguin tree-3-state-2) A3-state-2)
  (check-equal? (move-penguin tree-ran-move) '[[0 1] [2 1]] "take the greedy step"))
