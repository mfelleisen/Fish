#lang racket

;; hexagons as images

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

(require (only-in 2htdp/image image-color? mode? image?))

(provide/contract
 [hexagon 
  ;; create hexagon image (of size s)
  ;;
  ;;       (s,-s)       (2s,-s)
  ;;          *------------* 
  ;;         /              \
  ;;        /                \
  ;; (0,0) *                  * (3s,0)
  ;;        \                /
  ;;         \              /
  ;;          *------------*
  ;;       (s,+s)       (2s,+s)

  (-> natural-number/c mode? image-color? image?)]
 
 [hexagon-within
  ;; does this hexagon (of size s) contain point (x,y)?
  ;; the coordinates are relative to the anchor point (top left) of the hexagon's bounding box
  ;; 
  ;;    (0,0)
  ;;      +===*------------*===+
  ;;      |  /              \  |
  ;;      | /                \ |
  ;;      |*                  *|
  ;;      | \                / |
  ;;      |  \              /  |
  ;;      +===*------------*===+ (3s,3s)
  
  (->i ((s natural-number/c) (x natural-number/c) (y natural-number/c)) (r boolean?))])

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

(require (except-in 2htdp/image image-color? mode? image?))
(require lang/posn)

(module+ test
  (require rackunit))

;                                  
;                                  
;   ;                              
;   ;                              
;   ;                              
;   ; ;;;    ;;;;   ;;  ;;    ;;;  
;   ;;   ;  ;    ;   ;  ;    ;   ; 
;   ;    ;  ;;;;;;    ;;         ; 
;   ;    ;  ;         ;;     ;;;;; 
;   ;    ;  ;         ;;    ;    ; 
;   ;    ;  ;;   ;   ;  ;   ;   ;; 
;   ;    ;   ;;;;;  ;    ;   ;;; ; 
;                                  
;                                  
;                                  
;                                  

(define (hexagon s fill color)
  (polygon (hexagon-points s) fill color))

(define (hexagon-within s p.x p.y)  
  (cond
    [(<= (* 0 s) p.x (* 1 s)) (<= (- s p.x) p.y (+ s p.x))]
    [(<= (* 1 s) p.x (* 2 s)) (<= 0 p.y (* 2 s))]
    [(<= (* 2 s) p.x (* 3 s)) (define x (- p.x s s)) (<= x p.y (- (* 2 s) x))]
    [else #false]))

#; (Natural -> [Listof Posn])
;; determine the vertices of a hexagon 
(define (hexagon-points s)
  (list 
   (make-posn (* 1 s) (- s))
   (make-posn (* 2 s) (- s))
   (make-posn (* 3 s) 0)
   (make-posn (* 2 s) (+ s))
   (make-posn (* 1 s) (+ s))
   (make-posn (* 0 s) 0)))

#;
([hexagon* 
  ;; create hexagon image (of size s) with borderline colors 
  (-> natural-number/c mode? image-color? (and/c (listof image-color?) (compose (=/c 6) length))
      image?)])
;; fancy borders 
(define (hexagon* s fill color color*) (hexagon-with s fill color color*))

;; mark point (relative to top left of the bounding box) with cross hair to image
(define (hexagon-with s fill color color* (point #false))
  (cond
    ;; this can't happen 
    [(empty? color*) (hexagon s fill color)]
    [else (define points (hexagon-points s))
          (define rotate (append (rest points) (list (first points))))
          (for/fold ((base (hexagon s fill color))) ((p points) (q rotate) (c color*))
            (let* ([t base]
                   [t (place-image (circle 3 'solid 'orange) (posn-x p) (+ (posn-y p) s) t)]
                   [t (if point (place-image cross-hair (posn-x point) (posn-y point) t) t)]
                   [t (add-line t (posn-x p) (+ (posn-y p) s) (posn-x q) (+ (posn-y q) s) c)])
              t))]))

;; marking a point 
(define cross-hair
  (let* ([r 20]
         [s (circle r 'solid 'pink)]
         [s (add-line s r 0 r (* 2 r) 'black)]
         [s (add-line s 0 r (* 2 r) r 'black)])
    s))

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
  
  (check-true  (hexagon-within 100 100  17))
  (check-false (hexagon-within 100   0   0))
  (check-true  (hexagon-within 100  10 110))
  (check-false (hexagon-within 100  10 111))
  (check-true  (hexagon-within 100 110  10))
  (check-false (hexagon-within 100 200  -1))
  (check-true  (hexagon-within 100 210  10))
  (check-false (hexagon-within 100 210   9))
  (check-false (hexagon-within 100 300  99))
  (check-true  (hexagon-within 100 300 100))

  ; (hexagon-within 100 300 99)

  (check-true  (hexagon-within 10 10 0))
  (check-true  (hexagon-within 10 0 10))
  (check-false (hexagon-within 10 0 -10))
  (check-false (hexagon-within 10 -10 -10))
  (check-false (hexagon-within 10 20 -10))
  (check-true  (hexagon-within 10 30 10))
  (check-false (hexagon-within 10 29 0))
  (check-false (hexagon-within 10 30 -10)))

; (hexagon 20 'outline 'black)


#|
       (s,-s)       (2s,-s)
          *------------* 
         /              \
        /                \
 (0,0) *                  * (3s,0)
        \                /
         \              /
          *------------*
       (s,+s)       (2s,+s)
|#

(module+ test 
  (check-equal? (hexagon* 20 'solid 'white '(red black green blue yellow purple))
                (let* ([s 20]
                       [base (hexagon s 'solid 'white)]
                       ;; -- IN -- 
                       [base (add-line base (* 1 s) (* 0 s) (* 2 s) (* 0 s) 'red)]
                       [base (add-line base (* 2 s) (* 0 s) (* 3 s) (* 1 s) 'black)]
                       [base (add-line base (* 3 s) (* 1 s) (* 2 s) (* 2 s) 'green)]
                       [base (add-line base (* 2 s) (* 2 s) (* 1 s) (* 2 s) 'blue)]
                       [base (add-line base (* 1 s) (* 2 s) (* 0 s) (* 1 s) 'yellow)]
                       [base (add-line base (* 0 s) (* 1 s) (* 1 s) (* 0 s) 'purple)])
                  base)))
