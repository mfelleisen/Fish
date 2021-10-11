#lang racket

(provide
 ILL ;; ill-formed JSON 
 INV ;; invalid JSON 
 
 #; {[JSexpr -> Boolean] [JSexpr -> X ...] Symbol String -> X ...}
 get)

;; ---------------------------------------------------------------------------------------------------
(require Fish/Common/game-state)
(require SwDev/Testing/communication)

;; ---------------------------------------------------------------------------------------------------
(define ILL "ill-formed JSON")
(define INV "invalid JSON: ")

;; read a JSON value from current input port, validate and map to internal data ~~ or (error tag msg)
(define (get validator tag msg)
  (define x (read-message))
  (cond
    [(eof-object? x) (error tag "missing JSON")]
    [(and (string? x) (regexp-match #px"ERROR" x)) (error tag "~a:\n ~a" ILL x)])
  (define pieces (call-with-values (λ () (validator x)) list))
  (unless (first pieces) (error tag "~a ~a: ~e" INV msg x))
  (apply values pieces))
