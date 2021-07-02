#lang racket

(provide

 #; {[Listof String] -> [ImmutableHash 'port 'players]}
 configure-server

 #; {[Listof String] -> (values [Listof Player] [Listof String] [ [Listof Player] -> State])}
 ;; creates a list of players, their names, and a function that maps players to an initial game state 
 ;; the defaults are specified below 
 ;; the user can override them with commandline arguments xor a JSON config file 
 configure-game)

;; ---------------------------------------------------------------------------------------------------
(require Fish/Common/game-state)
(require Fish/Player/player)
(require Fish/Player/greedy)
(require json)

(module+ test
  (require rackunit)
  (require SwDev/Testing/check-values))


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

(define (configure-server c)
  (check-hash (get-hash-from-command-line c) tournament-defaults))
  


;                                            
;                                            
;     ;;                                     
;    ; ;                                     
;      ;           ;;;;  ;;;;  ;;;;;;   ;;;  
;      ;          ;;  ;      ; ;  ;  ; ;;  ; 
;      ;          ;   ;      ; ;  ;  ; ;   ;;
;      ;          ;   ;   ;;;; ;  ;  ; ;;;;;;
;      ;          ;   ;  ;   ; ;  ;  ; ;     
;      ;          ;; ;;  ;   ; ;  ;  ; ;     
;    ;;;;;         ;;;;   ;;;; ;  ;  ;  ;;;; 
;                     ;                      
;                  ;  ;                      
;                   ;;                       

(define NAMES '("Astrid" "Billybob" "Claus" "Doris"))

(define (configure-game c)
  (define-values (player#-or-list-of-strategy-files rows cols fish) (parse-args c))
  (define-values (names stra%)
    (cond
      [(number? player#-or-list-of-strategy-files)
       (define names (take NAMES player#-or-list-of-strategy-files))
       (define stra% (map (λ _ greedy-strategy) names))
       (values names stra%)]
      [(cons? player#-or-list-of-strategy-files)
       (define names (take NAMES (length player#-or-list-of-strategy-files)))
       (define stra% (map (λ (s) (dynamic-require s 'strategy%)) player#-or-list-of-strategy-files))
       (values names stra%)]
      [else (error 'configure "something went wrong: ~e" player#-or-list-of-strategy-files)]))
  (define plers (map (λ (s) (new player% [strategy (new s)])) stra%))
  (who-s-playing stra% names)
  (values plers names (configure-state rows cols fish)))

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

#; {N N (U N False) -> [ [Listof Player] -> State] }
(define [(configure-state rows cols fish) players]
  (if (boolean? fish)
      (create-state rows cols players)
      (create-state rows cols players #:fixed fish)))

#; {String -> (values (U N [Listof PathString]) N N (U False N))}
(define (parse-args n)
  (define h (check-hash (get-hash-from-command-line n) game-defaults))
  (values (dict-ref h 'players) (dict-ref h 'rows) (dict-ref h 'cols) (dict-ref h 'fish)))

;                              
;   ;                    ;     
;   ;                    ;     
;   ;                    ;     
;   ; ;;   ;;;;    ;;;   ; ;;  
;   ;;  ;      ;  ;   ;  ;;  ; 
;   ;   ;      ;  ;      ;   ; 
;   ;   ;   ;;;;   ;;;   ;   ; 
;   ;   ;  ;   ;      ;  ;   ; 
;   ;   ;  ;   ;  ;   ;  ;   ; 
;   ;   ;   ;;;;   ;;;   ;   ; 
;                              
;                              
;                              

#; {String -> ImmutableHash}
(define (get-hash-from-command-line c)
  (define k (string-join c " "))
  (cond
    [(regexp-match #px"--file (.*)" k)
     =>
     (λ (x)
       (define file (second x))
       (unless (file-exists? file)
         (error 'configure "path to config file expected, given ~a" file))
       (define h (with-input-from-file file read-json))
       (unless (hash? h)
         (error 'configure "object expected, given ~e" h))
       h)]
    [else (to-hash k)]))

#; {String -> ImmutableHash}
(define (to-hash x)
  (define not-in ".")
  (let* ([x (regexp-replace* #px"\\ *=\\ *" x not-in)]
         [x (~a "(" x ")")]
         [x (regexp-replace* #px"\\ +" x ")(")]
         [x (regexp-replace* (pregexp (~a "\\" not-in)) x " . ")])
    (define l (with-input-from-string x port->list))
    (make-immutable-hasheq (if (equal? '(()) l) '[] l))))

#; {ImmutableHash ImmutableHash -> ImmutableHash}
;; check that `h0` has all values keyed in defaults and they are in the specified range
;; or add a default value for this key 
(define (check-hash h0 defaults)
  (for/fold ([h h0]) ([(k v) defaults])
    (match-define (list default-value checker) v)
    (cond
      [(hash-ref h k #false) => (λ (in-h) (checker in-h) h)]
      [else (hash-set h k default-value)])))

;                                                          
;       ;            ;;                                    
;       ;           ;                  ;;;      ;          
;       ;           ;                    ;      ;          
;    ;;;;   ;;;   ;;;;;  ;;;;   ;   ;    ;    ;;;;;   ;;;  
;   ;; ;;  ;;  ;    ;        ;  ;   ;    ;      ;    ;   ; 
;   ;   ;  ;   ;;   ;        ;  ;   ;    ;      ;    ;     
;   ;   ;  ;;;;;;   ;     ;;;;  ;   ;    ;      ;     ;;;  
;   ;   ;  ;        ;    ;   ;  ;   ;    ;      ;        ; 
;   ;; ;;  ;        ;    ;   ;  ;   ;    ;      ;    ;   ; 
;    ;;;;   ;;;;    ;     ;;;;   ;;;;     ;;    ;;;   ;;;  
;                                                          
;                                                          
;                                                          

(define PORT 45678)

(define PLAYER# 2)
(define ROWS    6)
(define COLS    6)
(define FISH    #false)

#; {N N FormatString -> X -> N}
(define ((check-xyz low high err) w)
  (unless (or (and (natural? w) (<= low w high)) (boolean? w))
    (error 'configure err w))
  w)

(define game-defaults 
  (make-immutable-hasheq
   `[[rows . [,ROWS ,(check-xyz 2 9 "number of rows expected, given ~e")]]
     [cols . [,COLS ,(check-xyz 2 9 "number of columns expected, given ~e")]]
     [fish . [,FISH ,(check-xyz 1 5 "number of fish per tile expected, given ~e")]]
     [players . [,PLAYER# ,(or/c (listof any/c) (check-xyz 2 4 "players expected, given ~e"))]]]))

(define tournament-defaults
  (make-immutable-hash
   `[[port . [,PORT ,(check-xyz 10000 65000 "port expected, given ~e")]]
     [rows . [,ROWS ,(check-xyz 2 9 "number of rows expected, given ~e")]]
     [cols . [,COLS ,(check-xyz 2 9 "number of columns expected, given ~e")]]
     [fish . [,FISH ,(check-xyz 1 5 "number of fish per tile expected, given ~e")]]
     [players . [,PLAYER# ,(or/c (listof any/c) (check-xyz 2 100 "players expected, given ~e"))]]]))

  
;; ---------------------------------------------------------------------------------------------------
(module+ test
  (check-equal? (to-hash "players = 3") (make-immutable-hasheq '[[players . 3]]))
  (check-values (parse-args '["players = 3"]) 3 ROWS COLS #false "parse-arg player=3")
  (check-values (parse-args '["--file ../config-n.json"]) 4 5 6 2 "parse-arg file"))
