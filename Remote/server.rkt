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
(require Fish/Lib/hash-contract)

(define port/c (and/c natural-number/c (</c 60000) (>/c 10000)))
(define player#/c natural-number/c)

(define PORT 'port)
(define SERVER-TRIES 'server-tries)
(define SERVER-WAIT 'server-wait)
(define T-PLAYERS 't-players)
(define TIME-PER-TURN 'time-per-turn)
(define FISH 'fish)
(define ROWS 'rows)
(define COLS 'cols)

(define server-options (list PORT SERVER-TRIES SERVER-WAIT T-PLAYERS TIME-PER-TURN FISH ROWS COLS))

(provide
 ;; server options 
 PORT SERVER-TRIES SERVER-WAIT T-PLAYERS TIME-PER-TURN FISH ROWS COLS

 (contract-out
  [server
   #; (server player#/c wait-for-sec port#)
   ;; returns the list of winners and cheaters/failures 
   ;; runsning an manager on the players that connected on port# in time
   ;; plus the house players (if any) 
   ;; wait-for-sec seconds or N >= player# as soon as that many signed up 
   (->i ([config (hash-carrier/c server-options)]) ([players any/c] )
        (r results/c))]))

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
(require (except-in Fish/Admin/manager results/c))

(require SwDev/Testing/communication)

(module+ test
  (require (submod ".."))
  (require Fish/Remote/client)
  (require Fish/Player/player)
  (require Fish/Player/greedy)
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

(define test-run?  (make-parameter #false))
(define MIN-ERROR  "server did not sign up enough ~a players")

;; get at least min players, at most max players
;; wait for at most 30s
;; if during this interval, min players showed up: work with the players you have
;; otherwise, re-start the thread

;; threads plus channels: 
;; create thread and run for time/out
;; if timed out, tell thread "time's up"
;; -- if there are min players, return those and shut down
;; -- otherwise return false, requesting an extension

(define (server config [house-players '()])
  (define port (dict-ref config PORT))
  (define MAX-TIME    (dict-ref config SERVER-WAIT))
  (define MIN-PLAYERS (dict-ref config T-PLAYERS))
  (define MAX-PLAYERS (dict-ref config T-PLAYERS)) ;; BUG: need to accommodate max and min 
  (define MAX-TRIES   (dict-ref config SERVER-TRIES))

  ;; set up custodian so `server` can clean up all threads, TCP ports in case it is re-used
  (parameterize ([current-custodian (make-custodian)])
    (define players (wait-for-players port house-players MAX-TRIES MAX-TIME MIN-PLAYERS MAX-PLAYERS))
    (begin0 
      (cond
        [(empty? players) (fprintf (current-error-port) MIN-ERROR MIN-PLAYERS) DEFAULT-RESULT]
        [(test-run?) => (λ (result) (channel-put result players) DEFAULT-RESULT)]
        [else (configure-manager players config)])
      (custodian-shutdown-all (current-custodian)))))

#; {[Listof Player] ImmutableHash -> [List [Listof Player] [Listof Player]]}
(define (configure-manager players config)
  (define game-time-out (dict-ref config TIME-PER-TURN))
  (define fish#         (dict-ref config FISH))
  (define row#          (dict-ref config ROWS))
  (define col#          (dict-ref config COLS))
  (manager players #:time-out game-time-out #:fixed fish# #:size (list row# col#)))

#;{Port# [Listof Player] Int Int Int -> [Listof Player]}
(define (wait-for-players port house-players MAX-TRIES MAX-TIME MIN-PLAYERS MAX-PLAYERS)
  (define communicate-with-sign-up (make-channel))
  (thread (sign-up-players port communicate-with-sign-up house-players MIN-PLAYERS MAX-PLAYERS))
  (let loop ([n MAX-TRIES])
    (cond
      [(zero? n) '()]
      [(sync/timeout MAX-TIME communicate-with-sign-up) => reverse]
      [else
       (channel-put communicate-with-sign-up (~a "are there at least " MIN-PLAYERS " signed up"))
       (cond
         [(channel-get communicate-with-sign-up) => reverse]
         [else (loop (- n 1))])])))

#; {Port Channel [Listof Player] Int Int -> Void}
(define [(sign-up-players port send-players house-players MIN-PLAYERS MAX-PLAYERS)]
  (define listener (tcp-listen port MAX-TCP REOPEN))
  (let collect-players ([players house-players])
    (cond
      [(= (length players) MAX-PLAYERS)
       (channel-put send-players players)]
      [else
       (sync
        (handle-evt listener
                    (λ (_)
                      (collect-players (add-player players listener))))
        (handle-evt send-players
                    (λ (_)
                      (cond
                        [(>= (length players) MIN-PLAYERS) (channel-put send-players players)]
                        [else (channel-put send-players #false) (collect-players players)]))))])))

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
       (displayln (~a "failed to send " m) (current-error-port))
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

  (define config 
    (make-immutable-hash
     `[[port . 45670]
       [server-wait . 40]
       [t-players . 5]
       [server-tries . 1]
       [time-per-turn . 10]
       [rows . 5]
       [cols . 5]
       [fish . 2]
       [players . 4]]))


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
      (define config2
        (let* ([config config]
               [config (hash-set config PORT port)]
               [config (if k (hash-set config T-PLAYERS k) config)])
          config))
      (define th (thread (λ () (server config2))))
      (sleep 1)
      (if (boolean? k)
          (sync th)
          (for ([i k])
            (define-values (- +) (tcp-connect LOCAL port))
            (send-message "a" +))))
    (begin0
      (if k (channel-get result) (get-output-string err-out))
      (custodian-shutdown-all custodian)))

  (check-equal? (run-server-test 45671 #f) (format MIN-ERROR 5) "no sign ups")
  (check-equal? (run-server-test 45677 10) (build-list 10 add1) "sign up enough players")
  (check-equal? (run-server-test 45676 9) (build-list  9 add1) "sign up too few players"))

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
  (define PORT# 45674)
  (parameterize ([current-custodian (make-custodian)])
    (define players  (build-list (length PLRS) (λ _ (new player% [strategy (new greedy-strategy)]))))
    (define named    (map list PLRS players))
    (define o*       (open-output-string))
    (define config3
      (let* ([config config]
             [config (hash-set config PORT PORT#)]
             [config (hash-set config T-PLAYERS (- (length PLRS) 2))]) ;; bad players drop out 
        config))
    (define customer (thread (λ () (parameterize ([current-error-port o*]) (client named PORT#)))))
    (match-define [list winners cheaters] (parameterize ([current-error-port o*]) (server config3)))
    (sync customer)
    (begin0 (check-true (empty? (rest winners)))
            (check-true (empty? cheaters))
            (custodian-shutdown-all (current-custodian)))))
