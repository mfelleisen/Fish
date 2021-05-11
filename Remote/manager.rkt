#lang racket

;; a remote manager that connects a single player to a client system, which connects to a server 

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

(require (only-in Fish/Common/player-interface player/c))
(require (only-in json jsexpr?))

(define receiver/c (-> (-> (or/c eof-object? jsexpr?) jsexpr?) any))

(provide
 (contract-out
  [make-remote-manager
   (->i ([r receiver/c]) (#:broken (broken any/c)) (p (-> (or/c #f player/c) any/c)))]))

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

(require (submod Fish/Common/board serialize))
(require (submod Fish/Common/player-interface serialize))
(require (submod Fish/Common/game-state serialize))
(require Fish/Common/penguin)
(require SwDev/Testing/communication)

(module+ test
  (require (submod ".."))
  (require Fish/Player/player)
  (require (submod Fish/Common/game-state examples))
  (require Fish/Common/game-state)
  (require rackunit))

;                                                                               
;                                                        ;                      
;                                                        ;            ;         
;                                                        ;                      
;   ;;;;    ;;;;   ;;;   ;   ;  ;   ;         ;;;;    ;;;; ;;;;;;   ;;;   ; ;;  
;   ;; ;;   ;;  ; ;; ;;   ; ;   ;   ;             ;  ;; ;; ;  ;  ;    ;   ;;  ; 
;   ;   ;   ;     ;   ;   ;;;    ; ;              ;  ;   ; ;  ;  ;    ;   ;   ; 
;   ;   ;   ;     ;   ;    ;     ; ;           ;;;;  ;   ; ;  ;  ;    ;   ;   ; 
;   ;   ;   ;     ;   ;   ;;;    ; ;          ;   ;  ;   ; ;  ;  ;    ;   ;   ; 
;   ;; ;;   ;     ;; ;;   ; ;    ;;           ;   ;  ;; ;; ;  ;  ;    ;   ;   ; 
;   ;;;;    ;      ;;;   ;   ;    ;            ;;;;   ;;;; ;  ;  ;  ;;;;; ;   ; 
;   ;                             ;                                             
;   ;                            ;                                              
;   ;                           ;;

(define ((make-remote-manager receiver #:broken (broken #f)) player)
  (define done? (box (gensym)))
  (define r (if (not broken) (dispatcher done? player) (broken-dispatcher done? player broken)))
  (parameterize ([io-time-out 1000])
    (let loop ()
      (with-handlers ([void (λ (xn) (log-info (~a "manager caught ~a" xn)) (set-box! done? xn))])
        (receiver r)
        (unless (boolean? (unbox done?))
          (loop)))))
  (unbox done?))

#; {[Box Boolean] Player -> [JSexpr -> JSexpr]}
(define ((dispatcher done? p) input-received)
  (match input-received
    [(? eof-object?) #false]
    [`["playing-as" [,(? penguin-color/c as)]]          (void->jsexpr (send p playing-as as))]
    [`["playing-with" [[,(? penguin-color/c opp) ...]]] (void->jsexpr (send p playing-with opp))]
    [`["setup" [,[? statx s]]]                          (posn->jsexpr (send p initial (statx s)))]
    [`["take-turn" [,(? statx s) [,(? action? a) ...]]] (action->jsexpr
                                                         (send p take-turn
                                                               (statx s) (map jsexpr->action a)))]
    [`["start" [,(? boolean? r)]] (set-box! done? 'go)  (void->jsexpr (send p start-of-tournament r))]
    [`["end" [,(? boolean? r)]] (set-box! done? r)      (void->jsexpr (send p end-of-tournament r))]
    [other (error 'remote-manager "the server sent an ill-formed message: ~e" other)]))

(define (statx s)
  (state s #:soft #t))

(define (void->jsexpr _) "void")

(define ((broken-dispatcher done? p broken) input-received)
  (match input-received
    [(? eof-object?) #false]
    [`["playing-as" [,(? penguin-color/c as)]]          (void->jsexpr (send p playing-as as))]
    [`["playing-with" [[,(? penguin-color/c opp) ...]]] (void->jsexpr (send p playing-with opp))]
    [`["setup" [,[? statx s]]]                          (broken (send p initial (statx s)))]
    [`["take-turn" [,(? statx s) [,(? action? a) ...]]] (action->jsexpr
                                                         (send p take-turn
                                                               (statx s) (map jsexpr->action a)))]
    [`["start" [,(? boolean? r)]] (set-box! done? 'go)  (void->jsexpr (send p start-of-tournament r))]
    [`["end" [,(? boolean? r)]] (set-box! done? r)      (void->jsexpr (send p end-of-tournament r))]
    [other (error 'remote-manager "the server sent an ill-formed message: ~e" other)]))

;                                     
;                                     
;     ;                    ;          
;     ;                    ;          
;   ;;;;;   ;;;    ;;;   ;;;;;   ;;;  
;     ;    ;;  ;  ;   ;    ;    ;   ; 
;     ;    ;   ;; ;        ;    ;     
;     ;    ;;;;;;  ;;;     ;     ;;;  
;     ;    ;          ;    ;        ; 
;     ;    ;      ;   ;    ;    ;   ; 
;     ;;;   ;;;;   ;;;     ;;;   ;;;  
;                                     
;                                     
;                                     

(module+ test
  (define p1 (new player%))
  (define b1 (box (gensym)))

  (check-equal? ((dispatcher b1 p1) eof) #false)

  (check-equal? ((dispatcher b1 p1) `["playing-as" ["red"]]) "void")
  (check-true (symbol? (unbox b1)))

  (check-equal? ((dispatcher b1 p1) `["playing-with" [["red" "brown"]]]) "void")
  (check-true (symbol? (unbox b1)))

  (check-equal? ((dispatcher b1 p1) `["end" [#true]]) "void")
  (check-true (unbox b1))

  (set-box! b1 (gensym))
  (check-equal? ((dispatcher b1 p1) `["start" [#false]]) "void")
  (check-equal? (unbox b1) 'go)
  
  (check-true ((make-remote-manager (λ (f) (f `["end" [#true]]))) p1))

  (define 00-state-j (state->jsexpr (complete-state 00-state)))

  (define b*
    `[["start" [#true]]
      ["playing-as" ["red"]]
      ["playing-with" [["red" "brown"]]]
      ["setup" [,00-state-j]]
      ["take-turn" [,00-state-j []]]
      ["take-turn" [,00-state-j [[[0 0] [0 1]]]]]
      ["end" [#true]]])
  (check-true ((make-remote-manager (λ (f) (begin0 (f (first b*)) (set! b* (rest b*))))) p1))
  (check-pred exn:fail? ((make-remote-manager (λ (f) (f `[0 [#true]]))) p1)))
