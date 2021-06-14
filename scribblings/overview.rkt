#lang racket

#|

    +----------------------------+                           +----------------------------+
    | Client                     |                           | Server                     |
    +----------------------------+                           +----------------------------+
    | player mechanism           |                           | tournament manager         |
    | strategy                   | relies on      relies on  | referees                   |
    | GUI mechanism for people?  |-----------+  +------------| observers                  |
    +----------------------------+           |  |            +----------------------------+
                                             |  |
                                             v  v
                    +---------------------------------------------------------+
                    | the common ontology of Clients and Server               |
                    +---------------------------------------------------------+
                    | player interface and protocols                          |
                    | the rules of the game, expressed as complete game trees |
                    | formulated in terms of game states                      |
                    | which are made up of boards, fish, and penguins         |
                    +---------------------------------------------------------+
|#

(provide overview)

(require pict)

#; {PositiveReal -> Pict}
(define [overview s]
  (let* {[pic (vc-append 100 (hc-append 300 client server) onto)]
         [pic (connect pic server lc-find pi)]
         [pic (connect pic client rc-find 00)]
         [pic (pin-arrow-line 10 pic client rc-find server lc-find #:label (text "communicate via"))]
         [pic (pin-arrow-line 10 pic server lc-find client rc-find)]}
    (scale pic s)))

#; { Pict -> Pict}
(define (connect pic server lc-find aa)
  (pin-arrow-line 10 pic server lc-find onto ct-find
                  #:start-angle aa
                  #:start-pull .8
                  #:end-pull 0
                  #:end-angle (/ pi -2)
                  #:label rely))

#; {String String ... -> Pict}
(define (box title . lines)
  (define t-pict (text title))
  (define linesp (map text lines))
  (define widenp (widen (cons t-pict linesp)))
  (vl-append
   (frame (first widenp))
   (frame (apply vc-append (rest widenp)))))

#; {[Listof Pict] -> [Listof Pict]}
(define (widen lop)
  (define max-width (+ 4 (apply max (map pict-width lop))))
  (define blank-p   (blank max-width (+ (pict-height (first lop)) 4)))
  (map (λ (p) (hc-append (blank 5 1) (lc-superimpose p blank-p))) lop))

(define client (box "Client" "player mechanism" "strategy" "GUI mechanism for spectators"))

(define server (box "Server" "tournament manager" "referee" "observers"))

(define onto   
  (box "Common Ontology"
       "player interface and protocols"
       "the rules of the game"
       "-- formulated as trees"
       "-- in terms of game states"
       "-- which consist of boards and player descriptions"))

(define rely (text "relies on"))

(module+ test
  (overview 0.99))