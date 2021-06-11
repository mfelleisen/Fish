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
;                                   ;        
;                                   ;        
;                                   ;        
;    ;;;;   ;;;;   ;;;    ;;;    ;;;;  ;   ; 
;   ;;  ;   ;;  ; ;;  ;  ;;  ;  ;; ;;  ;   ; 
;   ;   ;   ;     ;   ;; ;   ;; ;   ;   ; ;  
;   ;   ;   ;     ;;;;;; ;;;;;; ;   ;   ; ;  
;   ;   ;   ;     ;      ;      ;   ;   ; ;  
;   ;; ;;   ;     ;      ;      ;; ;;   ;;   
;    ;;;;   ;      ;;;;   ;;;;   ;;;;    ;   
;       ;                                ;   
;    ;  ;                               ;    
;     ;;                               ;;    

(define greedy-strategy
  (class base-strategy%
    (super-new)
    
    (define/override (evaluate trn state)
      (fish-at (fishes-board (tree-current state)) (second trn)))
    
    (define/augment (choose the-max tie-breaker xvalue)
      '[])))

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

(define g (new greedy-strategy))
(define (place-penguin s) (send g place-penguin s))
(define (move-penguin t) (send g move-penguin t))

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
