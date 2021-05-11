#lang racket

;; represents the "ground truth" state of players:
;; -- what the referee knows about the players, 
;; -- what other players know about each other.

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

(require (only-in Fish/Common/penguin penguin/c)) 
(require (only-in Fish/Common/fish fish#/c)) 
(require (only-in Fish/Common/board posn/c))
(require (only-in pict pict?))

(provide
 (contract-out
  (iplayer? contract?)
  
  (create-player
   ;; set up an internal player representation, knowledge about players 
   (-> penguin/c any/c iplayer?))

  (upscore-player
   ;; increase this player's running score 
   (-> iplayer? fish#/c iplayer?))

  (+place-player
   ;; add an avatar location to this player's places 
   (-> iplayer? posn/c iplayer?))

  (move-player
   ;; move an avatar of this player from old to new
   (->i ([p iplayer?] [old (p) posn/c] [new posn/c])
        #:pre/name (old p) "avatar exists" (member old (iplayer-places p))
        [r (old) (and/c iplayer? (λ (np) (not (member old (iplayer-places np)))))]))

  (iplayer-penguin (-> iplayer? pict?))
  (iplayer-color   (-> iplayer? string?))
  (iplayer-places  (-> iplayer? (listof posn/c)))
  (iplayer-payload (-> iplayer? any))
  (iplayer-score   (-> iplayer? natural-number/c))))

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

(module+ test
  (require rackunit))

(module+ serialize
  (require SwDev/Lib/pattern-matching))

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

(struct iplayer [color penguin score places payload] #:prefab)
#; {type InternalPlayer = (player ColorString Penguin Score [Listof Posn/c])}
;; the player is represented by Penguin in the visual display, uses the specified color, has collected
;; `score` fish, its penguins occupy the `places`, and an external client may use `payload` for ANY/C

(define (create-player pc x)
  (match-define (list color penguin) pc)
  (iplayer color penguin 0 '[] x))

(define (upscore-player p delta)
  (struct-copy iplayer p [score (+ (iplayer-score p) delta)]))

(define (+place-player p place)
  (struct-copy iplayer p [places (cons place (iplayer-places p))]))

(define (move-player p old nu)
  (struct-copy iplayer p [places (cons nu (remove old (iplayer-places p)))]))

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

(module+ test

  (define basic-player (create-player '["red" dot] 'payload))

  (check-equal? (iplayer-score (upscore-player basic-player 5)) 5 "upscore")
  (check-equal? (iplayer-places (+place-player basic-player '[2 2])) '[[2 2]] "+place")

  (define +player (+place-player basic-player '[2 2]))
  
  (check-equal? (iplayer-places (move-player +player '[2 2] '[2 3])) '[[2 3]] "move"))

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
   COLOR SCORE PLACES
   
   (contract-out
    (player-validator (-> jsexpr? any))
    (jsexpr->player   (-> any/c #;"after you called board validator on this input" iplayer?))
    (player->jsexpr   (-> iplayer? jsexpr?))))

  (require (submod Fish/Common/board serialize))
  (require Fish/Common/penguin)

  (define COLOR 'color)
  (define SCORE 'score)
  (define PLACES 'places)
  
  (def/mp player [_ c s p]
    #'(hash-table
       (COLOR (? penguin-color/c c))
       (SCORE (? natural? s))
       (PLACES (list (? posn-validator p) (... ...)))))
  
  ;; does the JSON value satisfy the spec for boards: (board f)
  (define (player-validator j)
    (match j
      [[player c s p] #true]
      [j #false]))
  
  (define (jsexpr->player j)
    (match j
      [(player c s p) (iplayer c (cadar penguins) s p 'xternal)]))

  (define (player->jsexpr ip)
    (match-define (iplayer c _ s places _) ip)
    (make-hasheq `[ (,COLOR . ,c) (,SCORE . ,s) (,PLACES . ,(map posn->jsexpr places))])))