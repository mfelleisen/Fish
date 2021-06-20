#lang racket

(provide
 configure-state

 #; {[Listof String] -> (values [Listof Player] [Listof String] [ [Listof Player] -> State])}
 ;; creates a list of players, their names, # of rows, # of columns, constant # of fish
 ;; the defaults are specified below 
 ;; the user can override them with commandline arguments xor a JSON config file 
 configure)

;; ---------------------------------------------------------------------------------------------------
(require Fish/Common/game-state)
(require Fish/Player/player)
(require Fish/Player/greedy)
(require json)

(module+ test
  (require rackunit)
  (require SwDev/Testing/check-values)
  (require SwDev/Debugging/diff))

;; ---------------------------------------------------------------------------------------------------
(define NAMES '("Astrid" "Billybob" "Claus" "Doris"))
(define PLAYER# 2)
(define ROWS    6)
(define COLS    6)
(define FISH    #false)

(define (configure n)
  (define-values (player#-or-list-of-strategy-files rows cols fish) (parse-args n))
  (cond
    [(number? player#-or-list-of-strategy-files)
     (define names (take NAMES player#-or-list-of-strategy-files))
     (define plers (map (λ (n) (new player% [strategy (new greedy-strategy)])) names))
     (who-s-playing 'greedy names)
     (values plers names (configure-state rows cols fish))]
    [(cons? player#-or-list-of-strategy-files)
     (define names (take NAMES (length player#-or-list-of-strategy-files)))
     (define stra% (map (λ (s) (dynamic-require s 'strategy%)) player#-or-list-of-strategy-files))
     (define plers (map (λ (s) (new player% [strategy (new s)])) stra%))
     (who-s-playing stra% names)
     (values plers names (configure-state rows cols fish))]
    [else (error 'configure "something went wrong: ~e" player#-or-list-of-strategy-files)]))

#; {N N (U N False) -> [ [Listof Player] -> State] }
(define [(configure-state rows cols fish) players]
  (if (boolean? fish)
      (create-state rows cols players)
      (create-state rows cols players #:fixed fish)))

#; {String -> (values (U N [Listof PathString]) N N (U False N))}
(define (parse-args n)
  (define k (string-join n " "))
  (define h 
    (cond
      [(regexp-match #px"--file (.*)" k)
       =>
       [match-lambda
         [(list _ name)
          (with-input-from-file name read-json)]]]
      [else
       (to-hash k)]))
  (define player# (check-player# (dict-ref h 'players PLAYER#)))
  (define rows    (check-rows    (dict-ref h 'rows    ROWS)))
  (define cols    (check-cols    (dict-ref h 'columns COLS)))
  (define fish    (check-fish    (dict-ref h 'fish    FISH)))
  (values player# rows cols fish))

#; {(U Symbol [Listof FilePath]) [Listof String] -> Void}
(define (who-s-playing strts names)
  (displayln `[The following players are competing])
  (define lists-to-display
    (if (symbol? strts)
        (map (λ (n) `[,n with ,strts]) names)
        (map (λ (n s) `[,n with ,(object-name s)]) names strts)))
  (define lengths
    (apply max (map (λ (l) (apply + (map (compose string-length ~a) l))) lists-to-display)))
  (for-each displayln lists-to-display)
  (displayln (make-string (+ 4 lengths) #\-)))

#; {N N FormatString -> X -> N}
(define ((check-xyz low high err) w)
  (unless (or (and (natural? w) (<= low w high)) (boolean? w))
    (error 'xobserve err w))
  w)
(define (check-list w) (if (list? w) w #false))
(define check-player# (or/c check-list (check-xyz 2 4 "number of players expected, given ~e")))
(define check-rows    (check-xyz 2 9 "number of rows expected, given ~e"))
(define check-cols    (check-xyz 2 9 "number of columns expected, given ~e"))
(define check-fish    (check-xyz 1 5 "number of fish per tile expected, given ~e"))

#; {String -> Hash}
(define (to-hash x)
  (define not-in ".")
  (let* ([x (regexp-replace* #px"\\ *=\\ *" x not-in)]
         [x (~a "(" x ")")]
         [x (regexp-replace* #px"\\ +" x ")(")]
         [x (regexp-replace* (pregexp (~a "\\" not-in)) x " . ")])
    (define l (with-input-from-string x port->list))
    (make-hasheq (if (equal? '(()) l) '[] l))))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (check-false (diff (to-hash "players = 3") #hasheq[(players . 3)]) "to-hash player = 3")
  (check-values (parse-args '["players = 3"]) 3 ROWS COLS #false "parse-arg player=3"))
