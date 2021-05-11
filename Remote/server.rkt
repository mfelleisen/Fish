#lang racket

;; a tournament server that signs up players over TCP and runs a tournament 

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

(require (only-in Fish/Admin/manager results/c))

(define port/c (and/c natural-number/c (</c 60000) (>/c 10000)))
(define player#/c natural-number/c)
(define secs/c natural-number/c)
(define named-results/c [list/c [listof [listof string?]] [listof string?]])

(provide
 (contract-out
  [server
   #; (server player#/c wait-for-sec port#)
   ;; returns the list of winners and cheaters/failures 
   ;; runsning an manager on the N players that connected on port# in
   ;; wait-for-sec seconds or N >= player# as soon as that many signed up 
   (-> port/c results/c)]))

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

(require Fish/Remote/player)
(require Fish/Remote/basic-constants)
(require (except-in Fish/Admin/manager results/c))

(require SwDev/Testing/communication)

(module+ test
  (require (submod ".."))
  (require Fish/Remote/client)
  (require Fish/Player/player)  
  (require rackunit))

;                                            
;                                            
;                                            
;                                            
;    ;;;    ;;;    ;;;;  ;   ;   ;;;    ;;;; 
;   ;   ;  ;;  ;   ;;  ; ;   ;  ;;  ;   ;;  ;
;   ;      ;   ;;  ;      ; ;   ;   ;;  ;    
;    ;;;   ;;;;;;  ;      ; ;   ;;;;;;  ;    
;       ;  ;       ;      ; ;   ;       ;    
;   ;   ;  ;       ;       ;    ;       ;    
;    ;;;    ;;;;   ;       ;     ;;;;   ;    
;                                            
;                                            
;                                            

(define LOCAL     "127.0.0.1")
(define MAX-TCP   30)
(define REOPEN    #t)
(define DEFAULT-RESULT '[[] []])
(define TIME-PER-CALL 10.)

(define test-run?  (make-parameter #false))
(define MIN-ERROR  (~a "server did not sign up enough (" MIN-PLAYERS ") players"))

;; get at least min players, at most max players
;; wait for at most 30s
;; if during this interval, min players showed up: work with the players you have
;; otherwise, re-start the thread

;; threads plus channels: 
;; create thread and run for time/out
;; if timed out, tell thread "time's up"
;; -- if there are min players, return those and shut down
;; -- otherwise return false, requesting an extension

(define (server port)
  (define send-players (make-channel))
  (define custodian    (make-custodian))
  (define th
    (parameterize ([current-custodian custodian])
      (thread (sign-up-players port send-players))))
  (define players (wait-for-players send-players))
  (log-info "~a players are playing a tournament" (length players))
  (begin0
    (cond
      [(empty? players) (displayln MIN-ERROR (current-error-port)) DEFAULT-RESULT]
      [(test-run?) => (λ (result) (channel-put result players) DEFAULT-RESULT)]
      [else (manager players #:fixed 2 #:time-out TIME-PER-CALL)])
    (custodian-shutdown-all custodian)))

#;{Channel Thread -> [Listof Player]}
(define (wait-for-players send-players)
  (let loop ([n MAX-TRIES])
    (cond
      [(zero? n) '()]
      [(sync/timeout MAX-TIME send-players) => reverse]
      [else
       (channel-put send-players (~a "are there at least " MIN-PLAYERS " signed up"))
       (cond
         [(channel-get send-players) => reverse]
         [else (loop (- n 1))])])))

#; {Port Channel -> Void}
(define [(sign-up-players port send-players)]
  (define listener (tcp-listen port MAX-TCP REOPEN))
  (let collect-players ([players '()])
    (cond
      [(= (length players) MAX-PLAYERS)
       (channel-put send-players players)]
      [else
       (sync
        (handle-evt listener (λ (_) (collect-players (add-player players listener))))
        (handle-evt send-players (stop-or-resume collect-players send-players players)))])))

#; {[[Listof Player] -> Void] Channel [Listof Player] -> Void}
(define ((stop-or-resume collect-players send-players players) _)
  (cond
    [(>= (length players) MIN-PLAYERS) (channel-put send-players players)]
    [else (channel-put send-players #false) (collect-players players)]))

#; (TCP-Listener [Listof Player] -> [Listof Player])
(define (add-player players listener)
  (with-handlers ((exn:fail:network? (lambda (x) (log-error "connect: ~a" (exn-message x)) players)))
    (define-values (in out) (tcp-accept listener))
    (define name (read-message in))
    (cond
      [(short-string? name)
       (define next (if (test-run?) (add1 (length players)) (make-remote-player in out)))
       (cons next players)]
      [else
       (define m [if (and (string? name) (regexp-match #px"Timed" name)) "name" "not a short string"])
       (log-error (~a "failed to send " m))
       (close-input-port in)
       (close-output-port out)
       players])))

#; {Any -> Boolean}
(define (short-string? p)
  (and (string? p)
       (regexp-match #px"^[a-zA-Z]*$" p) 
       (<= 1 (string-length p) 12)))

;                                                                                      
;                                                                                      
;     ;       ;             ;                          ;                    ;          
;     ;                                                ;                    ;          
;   ;;;;;   ;;;  ;;;;;;   ;;;   ; ;;    ;;;;         ;;;;;   ;;;    ;;;   ;;;;;   ;;;  
;     ;       ;  ;  ;  ;    ;   ;;  ;  ;;  ;           ;    ;;  ;  ;   ;    ;    ;   ; 
;     ;       ;  ;  ;  ;    ;   ;   ;  ;   ;           ;    ;   ;; ;        ;    ;     
;     ;       ;  ;  ;  ;    ;   ;   ;  ;   ;           ;    ;;;;;;  ;;;     ;     ;;;  
;     ;       ;  ;  ;  ;    ;   ;   ;  ;   ;           ;    ;          ;    ;        ; 
;     ;       ;  ;  ;  ;    ;   ;   ;  ;; ;;           ;    ;      ;   ;    ;    ;   ; 
;     ;;;   ;;;;;;  ;  ;  ;;;;; ;   ;   ;;;;           ;;;   ;;;;   ;;;     ;;;   ;;;  
;                                          ;                                           
;                                       ;  ;                                           
;                                        ;;                                            

(module+ test ;; timing

  #; { N Port-Number (U False n:N) -> (U False [Listof 0]: (=/c (length) n))}
  #; (run-server-test m p k)
  ;; runs the server on port p, waitig for m players, but receiving k
  (define (run-server-test port k)
    [define custodian (make-custodian)]
    [define result    (make-channel)]
    [define err-out   (open-output-string)]
    (parameterize ([test-run?          result]
                   [current-custodian  custodian]
                   [current-error-port err-out])
      (define th (thread (λ () (server port))))
      (sleep 1)
      (if (boolean? k)
          (sync th)
          (for ([i k])
            (define-values (- +) (tcp-connect LOCAL port))
            (send-message "a" +))))
    (begin0
      (if k (channel-get result) (get-output-string err-out))
      (custodian-shutdown-all custodian)))
  
  (check-equal? (run-server-test 45678 #f) (string-append MIN-ERROR "\n") "no sign ups")
  (check-equal? (run-server-test 45679 10) (build-list 10 add1) "sign up enough players")
  (check-equal? (run-server-test 45679  9) (build-list  9 add1) "sign up too few players"))

;                                                                 
;      ;;                                                         
;     ;           ;;;    ;;;             ;                    ;   
;     ;             ;      ;             ;                    ;   
;   ;;;;;  ;   ;    ;      ;           ;;;;;   ;;;    ;;;   ;;;;; 
;     ;    ;   ;    ;      ;             ;    ;;  ;  ;   ;    ;   
;     ;    ;   ;    ;      ;             ;    ;   ;; ;        ;   
;     ;    ;   ;    ;      ;             ;    ;;;;;;  ;;;     ;   
;     ;    ;   ;    ;      ;             ;    ;          ;    ;   
;     ;    ;   ;    ;      ;             ;    ;      ;   ;    ;   
;     ;     ;;;;     ;;     ;;           ;;;   ;;;;   ;;;     ;;; 
;                                                                 
;                                                                 
;                                                                 


(module+ test

  (define PLRS '["a" "b" "c" "d" "e1" "failed attempt at Name" "e"])
  (define PORT 45678)
  (parameterize ([current-custodian (make-custodian)])
    (define players  (build-list (length PLRS) (λ _ (new player%))))
    (define named    (map list PLRS players))
    (define o*       (open-output-string))
    (define customer (thread (λ () (parameterize ([current-output-port o*]) (client named)))))
    (match-define [list winners cheats-and-failures] (server PORT))
    (sync customer)
    (begin0 (check-true (empty? (rest winners)))
            (check-true (empty? cheats-and-failures))
            (custodian-shutdown-all (current-custodian)))))
