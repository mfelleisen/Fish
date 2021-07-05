#lang racket

(provide
  LOCALHOST 
 
 #; {Port# -> Void}
 ;; runs the server locally on port `p`
 run-server

 #; {Port# [Listof Player] -> Void}
 run-clients 

 #; {[InputPort OutputPort -> Void] -> (values InputPort OutputPort [-> Void])}
 local-setup

 #; {N N -> Void}
 #; (report-results passsed total-test-count)
 report-results

 #; {-> Port#}
 get-starter-port

 #; {Port# -> Void}
 set-starter-port)

;; ---------------------------------------------------------------------------------------------------
(require Fish/Remote/server)
(require Fish/Remote/client)
(require SwDev/Testing/make-client)
(require SwDev/Testing/communication)

;; ---------------------------------------------------------------------------------------------------
(define LOCALHOST "127.0.0.1")
(define BASE 12345)
(define PORT-STARTER-FILE "port-starter-file.rktl")

;; ---------------------------------------------------------------------------------------------------
(define (run-server config #:house (house-players '[]))
  (match-define [list winners cheats-and-failures] (server config house-players))
  (send-message `[,(~a winners) ,(~a cheats-and-failures)]))

;; ---------------------------------------------------------------------------------------------------
(define (run-clients port players ip)
  (unless (port/c port)
    (error 'xclient "port number expected, given ~e" port))

  (define named
    (for/list ([p players] [i (in-naturals)])
      (define name (~a "P" (integer->string i)))
      (if (not p) (list #f #f) (list name p))))
  (client named port #t ip))

(define (integer->string i)
  (cond
    [(<= 0 i 9) (digit->string i)]
    [else
      (define mod (modulo i 10))
      (define rem (remainder i 10))
      (string-append (digit->string rem) (integer->string mod))]))

(define (digit->string i)
  (list-ref '["z" "o" "tw" "th" "fo" "fi" "si" "se" "e" "nine"] i))

;; ---------------------------------------------------------------------------------------------------
(define (local-setup f)
  (define cust (make-custodian))
  (define-values (in out) (make-pipe))
  (parameterize ([current-custodian cust])
    (thread
     (λ ()
       (parameterize ([current-input-port (open-input-string "")]
                      [current-output-port out])
         (f)))))
  (define (tear-down)
    (close-input-port in)
    (close-output-port out)
    (custodian-shutdown-all cust))
  (values in out tear-down))

;; ---------------------------------------------------------------------------------------------------
(define (report-results passed total-test-count)
  (displayln
   `((passed ,passed)
     (total ,total-test-count)
     (partial-score ,passed))))

;; ---------------------------------------------------------------------------------------------------
(define (get-starter-port)
  (if (file-exists? PORT-STARTER-FILE)
      (with-input-from-file PORT-STARTER-FILE read)
      BASE))
(define (set-starter-port port)
  (with-output-to-file PORT-STARTER-FILE (λ () (writeln port)) #:exists 'replace))
