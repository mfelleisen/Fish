#lang racket

;; a board representation for "Fish.com"

#; {type Board = [Listof [Listof [Option FishTile]]]} 
;; CONSTRAINTS All inner lists are of the same length. 
;; INTERPRETATION a Board is a rectangle of FishTiles and Falses (holes).
;; The outer list denotes the rows, the inner list are cells within a row. 

#| A column of height 4 has this shape: 
   _______
  / (0,0) \ _______
  \_______// (1,0) \
  / (2,0) \\_______/
  \_______// (3,0) \
           \_______/

 Its width is 1. The data representation of this column is

 [[ [0,0] ]
  [ [1,0] ]
  [ [2,0] ]
  [ [3,0] ] ]

 because it has four rows. 
|#

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

(require (only-in Fish/Common/fish fish#/c))
(require (only-in pict pict?))

(define board/c  (listof (listof any/c)))
(define posn/c   (list/c natural-number/c natural-number/c)) ;; row x column
(define routes/c any/c)

(define MAX-ROW 25)
(define MIN-ROW 1)
(define MAX-COLUMN 25)
(define MIN-COLUMN 1)
(define row/c (and/c natural? (>=/c MIN-ROW) (<=/c MAX-ROW)))
(define column/c (and/c natural? (>=/c MIN-COLUMN) (<=/c MAX-COLUMN)))

(provide

 MAX-ROW    ;; maximum number of rows 
 MAX-COLUMN ;; maximum number of columns

 #; {type Stepper = Board -> Posn || Board Posn -> (U False Posn)}
 #; {Board Stepper [N Posn -> (U False X)] #:reserved [Listof Posn] -> [Listof X]}

 board-lr-td
 board-traverse 

 (contract-out
  (posn/c   contract?)
  (board/c  contract?)
  (row/c    contract?)
  (column/c contract?)

  (make-board
   ;; create a `rows x columns` board with at least `1fish` tiles that displaay exactly one fish
   ;; do not place tiles at the `posn`s specified via holes; #:fixed means all tiles have N fish 
   (->i ([rows row/c][columns column/c])
        [#:+     (1fish (rows columns) (and/c natural? (<=/c (+ (quotient (* rows columns) 10) 1))))
         #:-     (holes [listof posn/c])
         #:fixed (same-number-of-fish-per-tile fish#/c)]
        (r board/c)))

  (add-row
   ;; add a row of holes xor tiles with fixed number fish on there
   (-> board/c natural? (or/c #false natural?) board/c))

  (board-rows (-> board/c natural?))
  (board-columns (-> board/c natural?))
  
  (fish-at
   ;; how many fish are on this board at the specified position
   (-> board/c posn/c (or/c 0 fish#/c)))
  
  (remove-tile
   ;; remove the tile at the specified position from this board; also return the number of fishes
   (->i ([b board/c] [p posn/c]) #:pre (b p) (fish-at b p) (values (_ natural?) [_ board/c])))

  (neighboring-tiles
   ;; all neighboring tiles of the specified posn on this board 
   (-> board/c posn/c [listof posn/c]))

  (all-possible
   ;; all tiles accessible via a straight line on this board,
   ;; starting from the given posn; 
   ;; reserved posns are blockers
   (->i ([b board/c][p posn/c]) (#:reserved [reserved [listof posn/c]]) [r routes/c]))
  
  (render-board
   ;; render this board 
   (->i ([b board/c])
        (#:+ [penguin-places [listof [cons/c pict? [listof posn/c]]]]
         #:color (color string?)
         #:arrow (arrow (list/c posn/c posn/c)))
        (r pict?)))
  
  (logic-posn->pict-center (-> posn/c posn/c))

  (any-pict-posn->locic-center
   ;; convert Pict position to logical center point of the tile, if it is contained; #f otherwise 
   (->i ([x-y posn/c] #:rows (rows natural-number/c) #:columns (columns natural-number/c))
        (values (posn (or/c #false posn/c)) (center (list/c natural-number/c natural-number/c)))))

  ;; type Posn = [List Natural Natural]
  (posn-row    (-> posn/c natural-number/c))
  (posn-column (-> posn/c natural-number/c))))

(module+ homework-only
  (provide

   #; {Board Natural Natural -> Pict}
   #; (illustrate-routes row column)
   ;; create image for possible moves starting from `(row, column)`
   illustrate-routes))

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

(require (except-in Fish/Common/fish fish#/c))
(require Fish/Lib/hexagon)
(require Fish/Lib/toint)
(require (except-in pict pict?))

(module+ test
  (require (submod ".."))
  (require SwDev/Testing/check-values)
  (require rackunit))

(module+ homework-only
  (require (submod "..")))

(module+ serialize
  (require SwDev/Lib/pattern-matching))

;                                                  
;                                                  
;   ;                          ;                   
;   ;                                              
;   ;                                              
;   ; ;;;     ;;;    ;;;;    ;;;      ;;;    ;;;;  
;   ;;  ;;   ;   ;  ;    ;     ;     ;   ;  ;    ; 
;   ;    ;       ;  ;          ;    ;       ;      
;   ;    ;   ;;;;;   ;;;;      ;    ;        ;;;;  
;   ;    ;  ;    ;       ;     ;    ;            ; 
;   ;;  ;;  ;   ;;  ;    ;     ;     ;   ;  ;    ; 
;   ; ;;;    ;;; ;   ;;;;    ;;;;;    ;;;    ;;;;  
;                                                  
;                                                  
;                                                  
;                                                  


(define posn-row first)
(define posn-column second)

(define (make-board rows# columns# #:+ (1fish0 0) #:- (holes '()) #:fixed (f #f))
  (define 1fish 1fish0)
  (define spots (* rows# columns#))
  (define ((populate r) c)
    (set! spots (- spots 1))
    (cond
      [f (set! 1fish 0) (fish-tile f)]
      [(member (list r c) holes)
       #false]
      [(<= spots 1fish)
       (set! 1fish (- 1fish 1))
       (fish-tile 1)]
      [else
       (define n (+ (random MAX-FISH) 1))
       (when (= n 1) (set! 1fish (- 1fish 1)))
       (fish-tile n)]))
  (define candidate (build-list rows# (λ (r) (build-list columns# (populate r)))))
  (if (<= 1fish 0) candidate (make-board rows# columns# #:+ 1fish0 #:- holes)))

(define (add-row board how-wide fish#-or-false)
  (define width   (board-columns board))
  (define new-row
    (list
     (build-list width (λ (i) (and fish#-or-false (< i how-wide) (fish-tile fish#-or-false))))))
  (append board new-row))

(define (board-rows b) (length b))
(define (board-columns b) (apply max (map length b)))

#; {Board N N -> (U Tile #f)}
(define (board-ref board row column)
  (and (good-position board row column) (list-ref (list-ref board row) column)))

#; {Board N N -> Boolean}
(define (good-position board row column)
  (and (<= 0 row (- (length board) 1))
       (<= 0 column (- (length (first board)) 1))))

(define (fish-at board p)
  (match-define (list row column) p)
  (define f (board-ref board row column))
  (if (boolean? f) 0 (fish-tile-n f)))

(define (remove-tile board p)
  (match-define (list row column) p)
  (define n (fish-tile-n (board-ref board row column)))
  (define vboard (map list->vector board))
  (vector-set! (list-ref vboard row) column #false)
  (values n (map vector->list vboard)))

;; ---------------------------------------------------------------------------------------------------
(module+ test

  (check-equal? (fish-at (make-board 2 2 #:fixed 2) (list (random 2) (random 2))) 2 "2 everywhere")

  (check-values (remove-tile (make-board 1 1 #:+ 1) '[0 0])
                1
                '((#f))
                "rm 1fish tile from 1x1 board"))

;                                                                  
;                                                                  
;                                                                  
;    ;;;;;    ;                                                    
;   ;;        ;                                                    
;   ;       ;;;;;;   ;;;;   ; ;;;   ; ;;;    ;;;;    ;;;;    ;;;;  
;   ;;        ;     ;    ;  ;;  ;;  ;;  ;;  ;    ;   ;;  ;  ;    ; 
;    ;;;;;    ;     ;;;;;;  ;    ;  ;    ;  ;;;;;;   ;      ;      
;        ;    ;     ;       ;    ;  ;    ;  ;        ;       ;;;;  
;             ;     ;       ;    ;  ;    ;  ;        ;           ; 
;   ;    ;    ;     ;;   ;  ;;  ;;  ;;  ;;  ;;   ;   ;      ;    ; 
;    ;;;;;     ;;;   ;;;;;  ; ;;;   ; ;;;    ;;;;;   ;       ;;;;  
;                           ;       ;                              
;                           ;       ;                              
;                           ;       ;                              
;                                                                  

#; {type Stepper = Board -> Posn || Board Posn -> (U False Posn)}

#; Stepper 
(define board-lr-td
  (case-lambda
    [(board) '[0 0]]
    [(board spot)
     (define width  (- (board-columns board) 1))
     (define height (- (board-rows board) 1))
     (match-define (list row col) spot)
     (cond
       [(< col width)  (list row (+ col 1))]
       [(< row height) (list (+ row 1) 0)]
       [else           #false])]))

(define (board-traverse board0 in-which-order f #:reserved (reserved '[]))
  (define board (remove-all board0 reserved))
  (let search-for-unocupied-non-hole-tiles ([spot* '()][spot (in-which-order board)])
    (define fish# (fish-at board spot))
    (define spot**
      (cond
        [(and (> fish# 0) (f fish# spot)) => (λ (x) (cons x spot*))]
        [else spot*]))
    (cond
      [(in-which-order board spot) => (curry search-for-unocupied-non-hole-tiles spot**)]
      [else (reverse spot**)])))

;                                          
;                                          
;                                          
;                                          
;                                          
;   ;;;;;;   ;;;;   ;    ;   ;;;;    ;;;;  
;   ;  ;  ; ;;  ;;  ;;  ;;  ;    ;  ;    ; 
;   ;  ;  ; ;    ;   ;  ;   ;;;;;;  ;      
;   ;  ;  ; ;    ;   ;  ;   ;        ;;;;  
;   ;  ;  ; ;    ;   ;;;;   ;            ; 
;   ;  ;  ; ;;  ;;    ;;    ;;   ;  ;    ; 
;   ;  ;  ;  ;;;;     ;;     ;;;;;   ;;;;  
;                                          
;                                          
;                                          
;                                          

(struct routes [nw no ne se so sw] #:prefab)
#; {type Routes = [routes Route Route Route Route Route]}
;; maximal pathways in the six feasible directions 
#; {type Route  = [Listof Posn]}
#; {type Posn   = [List N N]}
#; {type Dir    = [List Z Z]}

;; -- from -- even --- odd ---
#; [List Dir Dir]
(define nw '[[-1 -1] [-1  0]])
(define no '[[-2  0] [-2  0]])
(define ne '[[-1  0] [-1 +1]])
(define se '[[+1  0] [+1 +1]])
(define so '[[+2  0] [+2  0]])
(define sw '[[+1 -1] [+1  0]])

(define (neighboring-tiles board p)
  (match-define (list row column) p)
  (for*/list ([δ (list no ne se so sw nw)][q (in-value (step p δ))] #:when (> (fish-at board q) 0))
    q))

(define (all-possible board0 p #:reserved (reserved '[]))
  (match-define (routes nw no ne se so sw) (all-possible-routes board0 p #:reserved reserved))
  (append no ne se so sw nw))

#; {Board Posn #:reserved [Listof Posn] -> Routes}
(define (all-possible-routes board0 p #:reserved (reserved '[]))
  (define board (remove-all board0 reserved))
  (apply routes (map (λ (d) (trace board p d)) (list nw no ne se so sw))))

(provide remove-all)

#; {Board [Listof Posn] -> Routes}
(define (remove-all board0 r)
  (for/fold ([n 0] [board board0] #:result board) ([r r]) (remove-tile board r)))

#; {Board Posn Dir -> Route}
;; ASSUME the board has holes where the trace must stop 
(define (trace board start0 direction)
  (let trace ([start start0])
    (define next (step start direction))
    (cond
      [(off-board? board next) '()]
      [else (cons next (trace next))])))

#; {Board Posn -> Boolean}
(define (off-board? board position)
  (not (apply board-ref board position)))

#; {Board Posn Dir -> Posn}
;; step in the specified `direction` from the given `position`,
;; regardless of whether the result is a posn on the board 
(define (step position direction)
  (match-define (list r c) position)
  (match-define (list d e) (if (even? r) (first direction) (second direction)))
  (list (+ r d) (+ c e)))

;; ---------------------------------------------------------------------------------------------------
(module+ test ;; neighbors 
  (define b0-neighbors (make-board 5 3 #:fixed 1))
  (check-equal? (neighboring-tiles b0-neighbors '[0 0]) '[[1 0][2 0]])
  (check-equal? (neighboring-tiles b0-neighbors '[2 1]) '[[0 1][1 1][3 1][4 1][3 0][1 0]]))

(module+ test ;; all reachable 
  (check-equal? (all-possible (make-board 2 2 #:fixed 1) '[0 0]) '[[1 0]])
  (check-equal? (all-possible #:reserved '[[1 0]] (make-board 2 2 #:fixed 1) '[0 0]) '[]))

;                                                                          
;                                                                          
;                                ;                     ;                   
;                                ;                                         
;                                ;                                         
;    ;;;;    ;;;;   ; ;;;    ;;; ;   ;;;;    ;;;;    ;;;    ; ;;;    ;;; ; 
;    ;;  ;  ;    ;  ;;   ;  ;;  ;;  ;    ;   ;;  ;     ;    ;;   ;  ;;  ;; 
;    ;      ;;;;;;  ;    ;  ;    ;  ;;;;;;   ;         ;    ;    ;  ;    ; 
;    ;      ;       ;    ;  ;    ;  ;        ;         ;    ;    ;  ;    ; 
;    ;      ;       ;    ;  ;    ;  ;        ;         ;    ;    ;  ;    ; 
;    ;      ;;   ;  ;    ;  ;;  ;;  ;;   ;   ;         ;    ;    ;  ;;  ;; 
;    ;       ;;;;;  ;    ;   ;;; ;   ;;;;;   ;       ;;;;;  ;    ;   ;;; ; 
;                                                                        ; 
;                                                                    ;  ;; 
;                                                                     ;;;  
;                                                                          

(define BACKGROUND "gray")

(define (render-board board #:color (color BACKGROUND) #:arrow (arrow #false) #:+ (positions '[]))
  (define width  (+ 4 (* TILE-SIZE 4 (length (first board))) TILE-SIZE))
  (define height (+ 4 (* TILE-SIZE (+ (length board) 1))))
  (let* ([pict (filled-rectangle #:color color width height)]
         [pict (add-fish-and-penguin pict board positions)]
         [pict (if (boolean? arrow) pict (apply add-line #:with-arrow #t pict (apply append arrow)))])
    pict))

#; {Pict Board [Listof Posn] -> Pict}
(define (add-fish-and-penguin pict0 board penguin-positions)
  (for/fold ([pict pict0]) ([row board][r (in-naturals)])
    (for/fold ([pic pict]) ([cell row] [c (in-naturals)])
      (define-values (x y) (tile-position r c))
      (define cell-pic (if cell (tile-with-fish cell) (empty-tile BACKGROUND)))
      (pin-over pic x y (add-penguin cell-pic (list r c) penguin-positions)))))

#; {[Pict Posn [Listof [Cons Pict [Listof Posn]]] -> Pict]}
(define (add-penguin cell-pic0 position penguin-positions)
  (for/fold ([cell-pic cell-pic0]) ([p penguin-positions])
    (match-define (cons penguin position*) p)
    (if (member position position*)
        (cc-superimpose cell-pic penguin)
        cell-pic)))

#;{Pict Natural Natural Natural Natural  -> Pict}
(define (add-line s row0 column0 row1 column1 #:color (c "red") #:with-arrow (with #false))
  (define from (λ (_ _1) (apply values (logic-posn->pict-center (list row0 column0)))))
  (define to   (λ (_ _1) (apply values (logic-posn->pict-center (list row1 column1)))))
  (if with
      (pin-arrow-line 10 s s from s to #:color c #:line-width 3)
      (pin-line s s from s to #:color c #:line-width 3)))

;; convert row-column pair to x-y coordinate pair (center of tile at [row,column])
(define (logic-posn->pict-center p)
  (match-define [list r c] p)
  (define-values (x y) (tile-position r c))
  (list (toint (+ 2 x (* 1.5 TILE-SIZE))) (toint (+ 2 y TILE-SIZE))))

#; {N N ->* N N}
;; convert row-column pair to x-y coordinate pair (top left of tile at [row,column])
(define (tile-position row column)
  (define y (+ 2 (* TILE-SIZE row)))
  (if (odd? row)
      (values (+ 2 (* 4 TILE-SIZE column) (* 2 TILE-SIZE)) y)
      (values (+ 2 (* 4 TILE-SIZE column))                 y)))

;; ---------------------------------------------------------------------------------------------------
(module+ picts ;; check rendering 
  (render-board
   (make-board 5 5)
   #:arrow '[[0 0] [2 1]]
   #:+ (list (cons {filled-rectangle 30 30 #:color "yellow"} '([0 0]))))
  
  (render-board
   (make-board 5 5)
   #:arrow '[[0 0] [2 1]]
   #:+ (list (cons {filled-rectangle 30 30 #:color "yellow"} '([0 0])))))

(module+ homework-only
  (define (illustrate-routes board row0 col0)
    (define aaa  (all-possible-routes board (list row0 col0)))
    (match-define (routes nw no ne se so sw) aaa)
    (let* ([s (render-board board)]
           [s (for/fold ([s s]) ([a (rest (vector->list (struct->vector aaa)))])
                (for/fold ([s s]) ([x a]) (apply add-line s row0 col0 x)))])
      s))

  (define illustrated (illustrate-routes (make-board 4 3) 2 2))
  #;illustrated)

;                                                                                                  
;                                                                                                  
;    ;;;                       ;                                                               ;   
;      ;                                                                                           
;      ;                                                                                           
;      ;     ;;;;    ;;; ;   ;;;      ;;;           ;    ;   ;;;;            ;;; ;  ;    ;   ;;;   
;      ;    ;;  ;;  ;;  ;;     ;     ;   ;          ;;  ;;  ;    ;          ;;  ;;  ;    ;     ;   
;      ;    ;    ;  ;    ;     ;    ;                ;  ;   ;               ;    ;  ;    ;     ;   
;      ;    ;    ;  ;    ;     ;    ;                ;  ;    ;;;;           ;    ;  ;    ;     ;   
;      ;    ;    ;  ;    ;     ;    ;                ;;;;        ;          ;    ;  ;    ;     ;   
;      ;    ;;  ;;  ;;  ;;     ;     ;   ;            ;;    ;    ;          ;;  ;;  ;   ;;     ;   
;       ;;;  ;;;;    ;;; ;   ;;;;;    ;;;             ;;     ;;;;            ;;; ;   ;;; ;   ;;;;; 
;                        ;                                                       ;                 
;                    ;  ;;                                                   ;  ;;                 
;                     ;;;                                                     ;;;                  
;                                                                                                  

(define (any-pict-posn->locic-center x-y #:rows rows #:columns columns)
  (match-define `[,x ,y] x-y)
  (define (in-hexagon? row column)
    (define-values (top-x top-y) (tile-position row column))
    (define local-x (- x top-x))
    (define local-y (- y top-y))
    (and (positive? local-x) (positive? local-y) (hexagon-within TILE-SIZE local-x local-y)))

  (define place
    (for*/first ([row (in-range rows)] [column (in-range columns)] #:when (in-hexagon? row column))
      (list row column)))

  (cond
    [(boolean? place) (values #false '[0 0])]
    [else 
     (match-define (list cx cy) (logic-posn->pict-center place))
     (values place (list cx cy))]))

;; ---------------------------------------------------------------------------------------------------
(module+ test ;; check conversion of coordinates from pict to logical 
  #;{ N N -> Void}
  (define (check-x-y-conversion row1 col1)
    (match-define (list x1 y1) (logic-posn->pict-center (list row1 col1)))
    (check-values (any-pict-posn->locic-center (list x1 y1) #:rows 5 #:columns 5)
                  (list row1 col1)
                  (list x1 y1)
                  (~a row1 "-" col1)))

  (check-x-y-conversion 0 0)
  (check-x-y-conversion 3 4)

  (define O (list 0 0))
  (check-values (any-pict-posn->locic-center O #:rows 5 #:columns 5) #false '(0 0) "not a tile"))

;                                                                          
;                                                                          
;                              ;             ;;;       ;                   
;                                              ;                           
;                                              ;                           
;    ;;;;    ;;;;    ;;;;    ;;;      ;;;      ;     ;;;    ;;;;;;   ;;;;  
;   ;    ;  ;    ;   ;;  ;     ;     ;   ;     ;       ;        ;;  ;    ; 
;   ;       ;;;;;;   ;         ;         ;     ;       ;       ;;   ;;;;;; 
;    ;;;;   ;        ;         ;     ;;;;;     ;       ;      ;;    ;      
;        ;  ;        ;         ;    ;    ;     ;       ;     ;;     ;      
;   ;    ;  ;;   ;   ;         ;    ;   ;;     ;       ;    ;;      ;;   ; 
;    ;;;;    ;;;;;   ;       ;;;;;   ;;; ;      ;;;  ;;;;;  ;;;;;;   ;;;;; 
;                                                                          
;                                                                          
;                                                                          
;                                                                          

(module+ serialize

  (require (only-in json jsexpr?))

  (provide
   BOARD
   
   (contract-out
    (board-validator (-> jsexpr? any))
    (posn-validator  (-> jsexpr? any))
    (jsexpr->board   (-> any/c #;"call only after you called board validator on this input" board/c))
    (board->jsexpr   (-> board/c jsexpr?))
    (jsexpr->posn    (-> posn/c #;"call only after you called posn validator on this input" posn/c))
    (posn->jsexpr    (-> posn/c posn/c))))

  ;; -------------------------------------------------------------------------------------------------
  (define BOARD 'board)
  (define MAXS 25)

  (def/mp board [_ b] #'(and b [list [list (or (? zero?) (? fish#/c)) (... ...)] (... ...)]))
  (def/mp posn [_ row column] #'[list (? natural? row) (? natural? column)])
  
  ;; does the JSON value satisfy the spec for boards: (board f)
  (define (board-validator j)
    (match j
      [[board f]
       (cond
         [(ormap (λ (row) (> (length row) 0)) f)
          (define max-width (apply max (map length f)))
          (define size (* (length f) max-width))
          
          (or #true #;(<=  size MAXS)
              (and (displayln `[board too large: ,size] (current-error-port))
                   #false))]
         [else #false])]
      [j #false]))
  
  ;; does the JSON value satisfy the spec for positions: (posn row column)
  (define (posn-validator j)
    (match j
      [(posn row column) (list row column)]
      [j #false]))

  (define (jsexpr->board j)
    (define width (apply max (map length j)))
    (for/list ([r j])
      (append (map (λ (x) (if (zero? x) #false (fish-tile x))) r)
              (make-list (- width (length r)) #false))))

  (define jsexpr->posn values)
  (define posn->jsexpr values)
  
  (define (board->jsexpr b)
    (for/list ([row b])
      (for/list ([cell row])
        (if cell (fish-tile-n cell) 0)))))
