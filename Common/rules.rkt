#lang racket

;; the game rules, represented complete games as lazy game trees
;; -- that way a player's strategy can explore the tree in depth 

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

(require (only-in Fish/Common/game-state fishes? turn?))
(require (only-in pict pict?))

(provide

 #; {type Tree}
 tree-current

 (contract-out
  (tree?         contract?)
  (generate-tree (-> fishes? tree?))
  (final?        (-> tree? boolean?))
  (map-branches  (-> tree? any/c (-> turn? tree? any) (listof (list/c turn? any/c))))
  (noop?         (-> tree? boolean?))
  (noop          (-> tree? any))
  (tree-path     (-> tree? turn? ... tree?))

  (take-action
   ;; if the tree maps the pair of positions to a tree, return it; otherwise #false
   (-> tree? turn? (or/c #false tree?)))
 
  [render-tree
   ;; turn the first n layers of the tree into picts 
   (->i ([t tree?] [n natural?]) (#:scale [s (and/c real? positive?)]) (r [listof [listof pict?]]))]))

(module+ examples
  (provide tree0 tree1 tree3))

(define (map-branches t e f)
  (cond
    [(noop? t) (list (list (noop t) e))]
    [else 
     (for/list ([1map (tree-mapping t)])
       (match-define (list step next) 1map)
       (list step (f step [next])))]))
  
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

(require (except-in Fish/Common/game-state fishes? turn?))
(require Fish/Lib/suspension)
(require (except-in pict pict?))

(module+ examples
  (require (submod Fish/Common/game-state examples)))

(module+ test
  (require (submod ".."))
  (require (submod ".." examples))
  (require (submod Fish/Common/game-state examples))
  (require rackunit))

(module+ picts
  (require (submod ".." examples)))

;                                                                          
;                                                                          
;        ;                                                                 
;        ;            ;                                                    
;        ;            ;                                                    
;    ;;; ;    ;;;   ;;;;;;    ;;;            ;;;;    ;;;;   ; ;;;          
;   ;;  ;;   ;   ;    ;      ;   ;           ;;  ;  ;    ;  ;;  ;;         
;   ;    ;       ;    ;          ;           ;      ;;;;;;  ;    ;         
;   ;    ;   ;;;;;    ;      ;;;;;           ;      ;       ;    ;         
;   ;    ;  ;    ;    ;     ;    ;           ;      ;       ;    ;    ;;   
;   ;;  ;;  ;   ;;    ;     ;   ;;           ;      ;;   ;  ;;  ;;    ;;   
;    ;;; ;   ;;; ;     ;;;   ;;; ;           ;       ;;;;;  ; ;;;     ;;   
;                                                           ;              
;                                                           ;              
;                                                           ;              
;                                                                          

(struct tree [current mapping] #:prefab)
#; {type Tree   = [tree Fishes [Listof Branch]]}
#; {type Branch = [List Turn [Suspension Tree]]}
#; {type Turn   = (U SKIP [List Posn Posn])}
;; INTERPRETATION SKIP is a symbol that stands for "this player can't act, but others can"
;; INTERPRETATION A `Tree` represents the current game state,
;; a `Turn` represents a move of the penguin on the first Posn to th second,
;; and the thunk returns the `Tree` whose game state is the result of executing this action

;; ---------------------------------------------------------------------------------------------------
(define (generate-tree state0)
  #; {State -> Tree}
  (define (on-to-next state)
    (generate-tree (next-player state)))
  
  #; {State -> Tree}
  (define (generate-tree state)
    (define actions (all-possible-actions state))
    (define branches
      (if (skip? actions)
          (list (list actions (suspend (on-to-next state))))
          (for/list ([a actions])
            (list a (suspend (on-to-next (move-avatar state (first a) (second a))))))))
    (tree state branches))

  (if (empty? (fishes-players state0))
      (tree state0 '())
      (generate-tree state0)))

;; ---------------------------------------------------------------------------------------------------
(define (final? t)
  (empty? (tree-mapping t)))

(define (all-actions t)
  (map first (tree-mapping t)))

(define (noop? t)
  (skip? (caar (tree-mapping t))))

(define (noop t) (caar (tree-mapping t)))

;; ---------------------------------------------------------------------------------------------------
(define (take-action t action)
  (match-define (tree _ mapping) t)
  (define domain-element action)
  (cond
    [(empty? mapping) #false]
    [(noop? t)       (and (symbol? action) [(second (first mapping))])]
    [else 
     (for/first ([1map mapping] #:when (equal? (first 1map) domain-element))
       [(second 1map)])]))

;; ---------------------------------------------------------------------------------------------------
(define (tree-path tree0 . steps0)
  (let tree-path ([tree tree0] [steps steps0])
    (cond
      [(empty? steps) tree]
      [(skip? (first steps))
       (match-define (list tree+) (tree-unfold tree))
       (tree-path tree+ (rest steps))]
      [else
       (define from (caar steps))
       (define to (cadar steps))
       (match (tree-unfold tree #:from from #:to to)
         [(list tree+) (tree-path tree+ (rest steps))]
         [_
          (printf "stuck at ~e\n" `[,from ,to])
          (pretty-print steps0)
          (pretty-print (rest steps))

          (if (equal? tree tree0)
              (pretty-print `[equal trees])
              (pretty-print `[different trees ,tree0]))
          (pretty-print tree)
          (error 'tree-path "stuck")])])))

#; (->i ([t tree?]) (#:from (from posn/c) #:to (to posn/c)) (r (listof tree?)))
;; step through one level of the given tree along (from,to), (from,*), or *
;; ASSUME mapping is not just `skip`
(define (tree-unfold t #:from (from #false) #:to (to #false))
  (match-define (tree _ mapping) t)
  (filter-map (choose from to) mapping))

#; {Posn (U False Posn) -> [ Branch -> Tree]}
;; for a domain element of the shape [from,to] or [from,*], run the thunk to get the next tree
(define ((choose from to) 1map)
  (cond
    [(and from to)
     (match 1map
       [(list (list (? (curry equal? from)) (? (curry equal? to))) gt) [gt]]
       [1map #false])]
    [from
     (match 1map
       [(list (list (? (curry equal? from)) to) gt) [gt]]
       [1map #false])]
    [to
     (match 1map
       [(list (list from (? (curry equal? to))) gt) [gt]]
       [1map #false])]
    [else
     [(second 1map)]]))

;                                          
;                                          
;                                          
;     ;                       ;            
;     ;                       ;            
;   ;;;;;;   ;;;;    ;;;;   ;;;;;;   ;;;;  
;     ;     ;    ;  ;    ;    ;     ;    ; 
;     ;     ;;;;;;  ;         ;     ;      
;     ;     ;        ;;;;     ;      ;;;;  
;     ;     ;            ;    ;          ; 
;     ;     ;;   ;  ;    ;    ;     ;    ; 
;      ;;;   ;;;;;   ;;;;      ;;;   ;;;;  
;                                          
;                                          
;                                          
;                                          

(module+ examples
  (define tree0 (tree 2-state-no-action '[]))
  (define tree1 (generate-tree 2-state-1-action))
  (define tree3 (generate-tree 22-state)))

(module+ test
  
  (check-equal? (generate-tree 2-state-no-action) tree0 "generate no action")
  (check-equal? (tree-unfold tree0) '[]                 "unfold no action")

  (check-equal? (all-actions tree1) (list '[[0 0] [1 0]]) "all actions")
  (check-true (skip? (first (all-actions tree3)))      "all actions, skip")

  (check-true  (tree? (take-action tree1 '[[0 0] [1 0]]))      "action? #t")
  (check-false (take-action tree3 '[(1 0) (0 0)])              "action? #f")
  
  (check-true (tree? (tree-path tree1 '{[0 0] [1 0]}))    "tree-path")

  (define can-unfold-7-times
    (let* ([u (tree-unfold tree1)]
           [u (append-map tree-unfold u)]
           [u (append-map tree-unfold u)]
           [u (append-map tree-unfold u)]
           [u (append-map tree-unfold u)]
           [u (append-map tree-unfold u)]
           [u (append-map tree-unfold u)])
      u))
  
  (check-equal? (map tree-current (tree-unfold tree1 #:from '[0 0]))
                (map tree-current (tree-unfold tree1 #:to '[1 0]))  "tree unfold 1")
  (check-equal? (append-map tree-unfold (append-map tree-unfold (tree-unfold tree3))) '() "unfold 2")
  
  (check-true (noop? tree3)  "skip? tree3")
  (check-false (noop? tree1) "skip? tree1"))

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

(define (render-tree tree0 levels0 #:scale (s 1.0))
  (let breadth-first ([trees (list tree0)] [levels levels0])
    (cond
      [(or (empty? trees) (zero? levels)) '()]
      [else 
       (define images     (map (render-root s) trees))
       (define next-layer (append-map unfold1 trees))
       (cons images (breadth-first next-layer (- levels 1)))])))

#; {PositiveReal -> [Tree -> Pict]}
(define ((render-root s) tree)
  (define state (tree-current tree))
  (define-values (x y) (render-state state))
  (scale (hc-append 5 x y) s))

#; {Tree -> [Listof Tree]}
(define (unfold1 t)
  (define mapping (tree-mapping t))
  (for/list ([1map mapping]) [(second 1map)]))

(module+ picts
  (render-tree tree0 12)
  (render-tree tree1 12)
  (render-tree tree3 12))
