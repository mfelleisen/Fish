#lang racket/gui

;; a primitive tournament observer

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

(provide
 (contract-out
  (tournament-observer%
   (class/c
    ;; (NOTE I am using `any/c` for now but may change this to a class that support a `name` method 
    (init-field (players (listof any/c)))
    
    (show
     ;; initialize and (don't) show `this` observer 
     (->m boolean? any))
    (show-next-round
     ;; add a new round of games to `this` tournament visualization
     (->m (listof (listof any/c)) any))
    (show-winners
     ;; change the color of the given winners for the last round of games in `this` observer
     
     ;; TRACE
     ;; `winners` must be as long as `games` for the preceding call to `show-next-round`
     ;; and each list must be a "sublist" of the corresponding list given to `show-next-round`
     ;;
     
     (->m (listof (listof any/c)) any))))))

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

(require Fish/GUI/gcanvas)
(require Fish/Lib/toint)
(require pict)

(module+ pict
  (require (submod "..")))


;                                                          
;          ;                                               
;          ;                                               
;          ;                                               
;    ;;;   ;;;;    ;;;    ;;;    ;;;;  ;   ;   ;;;    ;;;; 
;   ;; ;;  ;; ;;  ;   ;  ;;  ;   ;;  ; ;   ;  ;;  ;   ;;  ;
;   ;   ;  ;   ;  ;      ;   ;;  ;      ; ;   ;   ;;  ;    
;   ;   ;  ;   ;   ;;;   ;;;;;;  ;      ; ;   ;;;;;;  ;    
;   ;   ;  ;   ;      ;  ;       ;      ; ;   ;       ;    
;   ;; ;;  ;; ;;  ;   ;  ;       ;       ;    ;       ;    
;    ;;;   ;;;;    ;;;    ;;;;   ;       ;     ;;;;   ;    
;                                                          
;                                                          
;                                                          

(define COMPETITOR "black")
(define GAME       "green")
(define WINNER     "red")

;; tournament style 
(define TOUR-V-DELTA 10)
(define TOUR-BOTTOM (blank 1 20))

;; game style 
(define GAME-H-DELTA 20)
(define GAME-MARGINS (blank 4 1))

;; name style
(define SIZE 22)
(define FONT 'roman)


(define tournament-observer%
  (class object% (init-field players)

    ;; this lives here in case we get names for players 
    (define (make-name n #:color [color COMPETITOR])
      (colorize (rotate (text (~a n) FONT SIZE) (/ pi 2)) color))

    (field

     [p+n+i (for/list ([p players] [i (in-naturals)]) (list p (make-name (+ i 1)) (+ i 1)))]
     [names (map second p+n+i)]
     [pict_ (pict_-from (length players) (games->pict (fake-games)))]
     [width (toint (pict-width pict_))]
     [height (toint (pict-height pict_))]
     [tours '()])

    (define fr (new frame% [label "TOURNAMENT OBESERVER"] [min-width width] [min-height height]))
    (define gc (new gcanvas% [parent fr] [pict0 pict_]))

    (define/public (show on-off)
      (send fr show on-off))

    #; {[Listof [Listof Player]] -> Void}
    (define/public (show-next-round games)
      (proper-games games))

    #; {[Listof [Listof Player]] -> Void}
    (define/public (show-winners winners)
      (define games (caar tours))
      (set! tours (rest tours))
      (proper-games games #:winners winners))
    
    #; {[Listof [Listof Player]] -> [Listof [Listof Pict]]}
    (define/private (proper-games games #:winners (winners (build-list (length games) (λ _ '[]))))
      (define proper 
        (for/list ([1game games][1winner winners])
          (for/list ([p 1game])
            (match-define (list _ pname pindex) (assoc p p+n+i))
            (if (member p 1winner)
                (make-name pindex  #:color WINNER)
                pname))))
      (define winner-pict (games->pict proper))
      (set! tours (cons (cons games winner-pict) tours))
      (define tours-pict (tournament->pict))
      (send gc set tours-pict))
        
    
    #; {-> Pict}
    (define (tournament->pict)
      (define apict (vc-append (apply vc-append TOUR-V-DELTA (map cdr tours)) TOUR-BOTTOM))
      (define frame  (rectangle (+ width 8) (+ height 8)))
      (cb-superimpose frame apict))

    #; {-> [Listof [Listof X]]}
    (define/private (fake-games)
      (define names# (length names))
      (define last-1 (remainder names# 4))
      (unless (= last-1 0)
        (set! names (append (build-list (- 4 last-1) (λ (x) (make-name (+ x 111)))) names)))
      (let loop ([names names])
        (cond
          [(empty? names) '()]
          [else (cons (take names 4) (loop (drop names 4)))])))

    (super-new)))

;                              
;                              
;                              
;                              
;   ;;;;   ;   ;  ;   ;        
;       ;  ;   ;   ; ;         
;       ;  ;   ;   ;;;         
;    ;;;;  ;   ;    ;          
;   ;   ;  ;   ;   ;;;         
;   ;   ;  ;   ;   ; ;    ;;   
;    ;;;;   ;;;;  ;   ;   ;;   
;                              
;                              
;                              

#; {[Listof [Listof Pict]] -> Pict}
(define (games->pict games)
  (define names (apply append games))
  (define height (apply max (map pict-height names)))
  (define width  (apply max (map pict-width names)))
  (define frame  (rectangle (+ width 4) (+ height 4) #:border-color COMPETITOR))
  (define game-picts (map (game->pict frame) games))
  (define one-game (apply hc-append GAME-H-DELTA game-picts))
  one-game)

#; {[listof Pict] -> Pict}
(define ((game->pict f) g)
  (define p (apply hc-append 3 (map (λ (g) (ct-superimpose (vc-append (blank 3) g) f)) g)))
  (define fr (rectangle (+ (pict-width p) 4) (+ (pict-height p) 4) #:border-color GAME))
  (hc-append GAME-MARGINS (cc-superimpose fr p) GAME-MARGINS))

#; {N Pict -> Pict}
(define (pict_-from n apict)
  (define width  (pict-width apict))
  ;; magic to make the window not too tall 
  (define height (* (+ 10 (pict-height apict)) (toint (+ 3 (log n 4))))) 
  (define frame  (rectangle (+ width 8) (+ height 8)))
  frame)

;                                     
;                                     
;             ;            ;          
;                          ;          
;   ;;;;    ;;;    ;;;   ;;;;;   ;;;  
;   ;; ;;     ;   ;;  ;    ;    ;   ; 
;   ;   ;     ;   ;        ;    ;     
;   ;   ;     ;   ;        ;     ;;;  
;   ;   ;     ;   ;        ;        ; 
;   ;; ;;     ;   ;;       ;    ;   ; 
;   ;;;;    ;;;;;  ;;;;    ;;;   ;;;  
;   ;                                 
;   ;                                 
;   ;                                 

(module+ picts
  (define players0 (build-list 32 values))
  (define tm (new tournament-observer% [players players0]))
  (send tm show #true)

  (define (run1 players0)
    (define games1 (let loop ([p players0]) (if (null? p) '[] (cons (take p 4) (loop (drop p 4))))))
    (send tm show-next-round games1)
    (define winners (map (compose list first) games1))
    (send tm show-winners winners)
    (apply append winners))

  (define players1 (run1 players0))
  (define players2 (run1 players1))
  #;(define players3 (run1 players2))
  #;(run1 (append (take players0 2) players3)))
