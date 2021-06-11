#lang racket

;; specify a strategt class with contract 

;; A strategy is defined as two methods:
;; -- one for helping a player pick a place during the placement phase of the game
;; -- another for helping a player move a penguin to a new place during the proper playing phase. 

;; also provide some common auxiliaries for defining strategies:
;; -- a selection function for filtering results compared to some value 
;                                                          
;                                                          
;                   ;                                ;;;   
;                   ;                                  ;   
;    ;;;   ;   ;  ;;;;;   ;;;    ;;;;  ; ;;   ;;;;     ;   
;   ;;  ;   ; ;     ;    ;;  ;   ;;  ; ;;  ;      ;    ;   
;   ;   ;;  ;;;     ;    ;   ;;  ;     ;   ;      ;    ;   
;   ;;;;;;   ;      ;    ;;;;;;  ;     ;   ;   ;;;;    ;   
;   ;       ;;;     ;    ;       ;     ;   ;  ;   ;    ;   
;   ;       ; ;     ;    ;       ;     ;   ;  ;   ;    ;   
;    ;;;;  ;   ;    ;;;   ;;;;   ;     ;   ;   ;;;;     ;; 
;                                                          
;                                                          
;                                                          

(require (only-in Fish/Common/rules tree?))
(require (only-in Fish/Common/game-state fishes? turn? move/c))
(require (only-in Fish/Common/board posn/c))

(provide
 (contract-out
  (base-strategy%
   (class/c
    (place-penguin
     ;; place a penguin on an available position, searching from the origin going right, then down
     (->m fishes? posn/c))

    (move-penguin
     ;; SANITY CHECK: the color of this player is the color of the first player in the state
     ;; reteturn action, lexicograpphically closest to ORIGIN
     #; #false           ;; -- when the state is final
     #; turn?            ;; -- when a player can skip or move 
     (->m tree? (or/c #false turn?)))

    (inner
     ;; both place-penguin and move-penguin call into the `choose` hierarchy 
     (choose
      ;; it hands the inner function
      ;; -- the max of the list of "valued" elements
      ;; -- the list oe "valued" elements
      ;; and expects the list of alternative candidates
      ;; If this resuult is empty, `choose` picks an element of the maximum-valued elements
      (->m real? [listof [list/c any/c real?]] any/c)))
     
    (evaluate
     ;; abstract: `move-penguin` calls this function to assess the value of a new "fish island"
     ;; determines the value of a turn that ends up in the given tree situation 
     {->m turn? tree? (and/c real? (compose not negative?))})))

  (select
   #;(select r cmp l)
   ;; select the elements x of l for which (cmp r x) holds 
   (-> real? (-> real? real? boolean?) (listof (list/c any/c real?)) (listof any/c)))))

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

(require (except-in Fish/Common/rules tree?))
(require (except-in Fish/Common/game-state fishes? turn? move/c))
(require (except-in Fish/Common/board posn/c))

(module+ test
  (require rackunit))

;                                                          
;                                                          
;            ;                    ;                        
;            ;                    ;                        
;    ;;;   ;;;;;   ;;;;  ;;;;   ;;;;;   ;;;    ;;;;  ;   ; 
;   ;   ;    ;     ;;  ;     ;    ;    ;;  ;  ;;  ;  ;   ; 
;   ;        ;     ;         ;    ;    ;   ;; ;   ;   ; ;  
;    ;;;     ;     ;      ;;;;    ;    ;;;;;; ;   ;   ; ;  
;       ;    ;     ;     ;   ;    ;    ;      ;   ;   ; ;  
;   ;   ;    ;     ;     ;   ;    ;    ;      ;; ;;   ;;   
;    ;;;     ;;;   ;      ;;;;    ;;;   ;;;;   ;;;;    ;   
;                                                 ;    ;   
;                                              ;  ;   ;    
;                                               ;;   ;;    

(define base-strategy%
  (class object%
    (super-new)

    ;; use left-to-right, top-down traversal to find the first highest-value spot
    (define/public (place-penguin s)
      (define spot* (state-board-traverse s board-lr-td (λ (x y) (list y x))))
      (when (empty? spot*)
        (error 'place-penguin "not enough spots for placing penguins"))
      (choose first spot*))

    (define/public (move-penguin t)
      (cond 
        [(final? t) #false]
        [else
         (define steps+value (map-branches t 0 (λ (trn tree) (evaluate trn tree))))
         (choose tie-breaker steps+value)]))

    (define/public (evaluate trn tree)
      0)

    (define/pubment (choose tie-breaker steps+value)
      (define the-max (max-map second steps+value))
      (define others (inner (error 'base-strategy% "inner missing") choose the-max steps+value))
      (if (empty? others) (tie-breaker (select the-max = steps+value)) (random-choice others)))))

(define (max-map f lox) (apply max (map f lox)))

(define (select the-max = fish-steps)
  (filter-map (λ (x) (and (= (second x) the-max) (first x))) fish-steps))

#; {[NEListof X] -> X}
(define (random-choice lst)
  (list-ref lst (random (length lst))))



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



#;
(tie-breaker
 ;; implemen a tie breaker if there are serveal equally valued player actions:
 ;; in order, apply the following "filters" to reduce the list:
 ;; top-most row of `from` field, `left-most` column of `from`, top-most for `to`, left-most for to
 (-> (and/c (listof turn?) cons?) turn?))

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
