#lang racket

;; a client that signs up some players with a server at a given IP address
;; and port, and then participates in a distributed tournament 

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
(require (only-in SwDev/Testing/make-client port/c))

(provide
 (contract-out
  [client 
   #; (client players ip port# wait?)
   ;; runs a client that connects all players to a server at ip on port#
   ;; waits for all of them if wait? is #t -- NEEDED FOR INDEPENDENT
   ;; RUNS of the client in a shell process 
   (->* ([listof (or/c [list/c #f #f]
                       [list/c string? player/c]
                       [list/c string? (list/c player/c any/c)])])
        (port/c boolean? string?)
        any)]))

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

(require Fish/Remote/manager)
(require Fish/Remote/basic-constants)
(require (except-in SwDev/Testing/make-client port/c))
(require SwDev/Testing/communication)

;                                                  
;                                                  
;            ;;;       ;                           
;              ;                              ;    
;              ;                              ;    
;     ;;;      ;     ;;;     ;;;;   ; ;;;   ;;;;;; 
;    ;   ;     ;       ;    ;    ;  ;;   ;    ;    
;   ;          ;       ;    ;;;;;;  ;    ;    ;    
;   ;          ;       ;    ;       ;    ;    ;    
;   ;          ;       ;    ;       ;    ;    ;    
;    ;   ;     ;       ;    ;;   ;  ;    ;    ;    
;     ;;;       ;;;  ;;;;;   ;;;;;  ;    ;     ;;; 
;                                                  
;                                                  
;                                                  
;                                                  

(define LOCAL "127.0.0.1")
(define PORT0 45678)

(define (client players (port PORT0) (wait? #false) (ip LOCAL))
  (define (connector name)
    (if (not name)
        (connect-to-server-as-receiver ip port)
        (connect-to-server-as-receiver ip port #:init (λ (ip) (send-message name ip)))))
  (define player-threads (make-players wait? players connector))
  (when wait?
    (wait-for-all player-threads)
    (displayln "all done")))

#; {type ChanneledThreads = [Listof [List Channel String  Thread]]}

#; {Boolean [Listof Player] [-> (values InputPort OutputPort)] -> ChanneledThreads}
(define (make-players wait? named-and-tagged-players connector)
  (define done (make-channel))
  (for/list ((p named-and-tagged-players) (i (in-naturals)))
    (when (= i 5)
      (sleep (/ MAX-TIME 2)))
    (define-values (name behavior manager)
      (match p
        [[list name (list behavior broken)] 
         (define-values (receiver _) (connector name))
         (values name behavior (make-remote-manager receiver #:broken broken))]
        [[list name behavior] 
         (define-values (receiver _) (connector name))
         (values name behavior (make-remote-manager receiver))]))
    (list done
          name
          (thread
           (λ ()
             (parameterize ([prefix-with-spaces 2040]
                            [trickle-output? #t])
               (define r (manager behavior))
               (if wait? (channel-put done (list name r)) (void))))))))

#; {ChanneledThreads -> Void}
;; display the results 
(define (wait-for-all player-threads)
  (when (cons? player-threads)
    (define removes-itself
      (for/list ((dp player-threads))
        (match-define [list done name th] dp)
        (handle-evt done (λ (r) (log-info "~a" r) (wait-for-all (remq dp player-threads))))))
    (apply sync removes-itself)))
