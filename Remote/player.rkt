#lang racket

;; this remote player implements the same interface as the player but conveys its arguments
;; to the given TCP out stream and receives the results on the TCP in stream

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

(require Fish/Common/player-interface)

(provide
 ;; a contract that describes the player class's interface to the administrator 
 (contract-out
  (make-remote-player (-> input-port? output-port? player/c))))

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

(require (submod Fish/Common/game-state serialize))
(require (submod Fish/Common/board serialize))
(require (submod Fish/Common/player-interface serialize))
(require (submod Fish/Common/penguin serialize))
(require json)

(require SwDev/Testing/communication)
(require (for-syntax syntax/parse))

(module+ test
  (require (submod ".."))
  (require (submod Fish/Common/game-state examples))
  (require Fish/Common/game-state)
  (require SwDev/Debugging/diff)
  (require rackunit))

;                                            
;                                            
;          ;;;                               
;            ;                               
;   ;;;;     ;    ;;;;   ;   ;   ;;;    ;;;; 
;   ;; ;;    ;        ;  ;   ;  ;;  ;   ;;  ;
;   ;   ;    ;        ;   ; ;   ;   ;;  ;    
;   ;   ;    ;     ;;;;   ; ;   ;;;;;;  ;    
;   ;   ;    ;    ;   ;   ; ;   ;       ;    
;   ;; ;;    ;    ;   ;   ;;    ;       ;    
;   ;;;;      ;;   ;;;;    ;     ;;;;   ;    
;   ;                      ;                 
;   ;                     ;                  
;   ;                    ;;                  


(define (make-remote-player in out)
  (new remote-player% [in in] [out out]))

(define remote-player%
  (class object% [init-field in out (strategy 'just-to-satisfy-the-contract)]
    (super-new)

    (define-syntax (define/remote stx)
      (syntax-parse stx
        [(_ ((m m-str) [->to]) <-from)
         #'(define/public (m x) (send-json 'm `[m-str [,(map ->to x)]] <-from))]
        [(_ ((m m-str) ->to ...) <-from)
         #:with (x ...) (generate-temporaries #'(->to ...))
         #'(define/public (m x ...) (send-json 'm `[m-str [,(->to x) ...]] <-from))]))

    (define/private (send-json tag json <-from)
      (send-message json out)
      (define msg (read-message in))
      (with-handlers ([exn:misc:match?
                       (λ (xn)
                         (log-error "~a: wrong return value: ~e" tag msg)
                         (log-error (exn-message xn))
                         (raise xn))])
        (<-from msg)))

    ;; -----------------------------------------------------------------------------------------------
    (define/remote ((start-of-tournament "start") boolean->jsexpr) jsexpr->void)
    (define/remote ((playing-as "playing-as") avatar->jsexpr) jsexpr->void)
    (define/remote ((playing-with "playing-with") [avatar->jsexpr]) jsexpr->void)
    (define/remote ((end-of-tournament "end") boolean->jsexpr) jsexpr->void)
    (define/remote ((initial "setup") state->jsexpr) jsexpr->posn)
    (define actions (λ (a) (map action->jsexpr a)))
    (define/remote ({take-turn "take-turn"} state->jsexpr actions) jsexpr->action)))


(define (boolean->jsexpr b)
  (cond
    [(boolean? b) b]
    [else (error 'boolean->jsexpr "~e" b)]))

(define (jsexpr->void j) (match j ["void" (void)]))

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
  
  (define-syntax (test stx)
    ; #:remote JSexpr ~~ what the client responds with
    ; #:exp JSexpr    ~~ ahat we expect the method to produce in response
    ; #:msg JSexpr    ~~ what the method sends to the client 
    (syntax-parse stx 
      [(test (method args ...)
             (~optional (~seq #:remote input) #:defaults ([input #'(~a "void")]))
             (~optional (~seq #:exp e) #:defaults ([e #'(void)]))
             #:msg m)
       #'(check-pred
          (λ (x) (cond [(diff x m) => (λ (δ) (pretty-print `[,x ,m ,δ]))][else #t]))
          (string->jsexpr
           (with-output-to-string
             (λ ()
               (check-equal? 
                (with-input-from-string (jsexpr->string input)
                  (λ ()
                    (define ip (current-input-port))
                    (define op (current-output-port))
                    (define r1 (make-remote-player ip op) #;(new remote-player% [in ip] [out op]))
                    (send r1 method args ...)))
                e)))))]))

  (define-syntax (texn stx)
    (syntax-parse stx 
      [(_ msg (method args ...) (~optional (~seq #:remote input) #:defaults ([input #'(~a "void")])))
       #'(check-exn
          exn:misc:match?
          (λ ()
            (with-input-from-string (if (string? input) input (jsexpr->string input))
              (λ ()
                (with-output-to-string
                  (λ ()
                    (define ip (current-input-port))
                    (define op (current-output-port))
                    (define r1 (new remote-player% [in ip] [out op]))
                    (send r1 method args ...))))))
          msg)]))
  
  (test (start-of-tournament #t)        #:msg '["start" [#t]])
  (test (playing-as "brown")            #:msg '["playing-as" ["brown"]])
  (test (playing-with '["red" "brown"]) #:msg '["playing-with" [["red" "brown"]]])
  (test (end-of-tournament #f)          #:msg '["end" [#f]])
  (test [initial 00-state] #:remote '[1 1] #:exp '[1 1] #:msg `["setup"  [,(state->jsexpr 00-state)]])

  (define a1 '[[1 1] [2 2]])
  (define s1 (next-player 22-state))
  (define j1 (state->jsexpr s1))
  (test [take-turn s1 '[]] #:remote a1  #:exp a1 #:msg `["take-turn" [,j1 []]])

  ;; --- make sure the return-values are well-formated JSON 
  (texn "take turn [[1 1]]" [take-turn s1 '[]] #:remote '[[1 1]])
  (texn "take turn [[1 1"   [take-turn s1 '[]] #:remote "[[1 1"))
