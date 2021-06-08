#lang racket

;; specify a struct-based contract for strategies

;; A strategy is defined as a pair of functions:
;; -- one for helping a player pick a place during the placement phase of the game
;; -- another for helping a player move a penguin to a new place during the proper playing phase. 

(require (only-in Fish/Common/game-tree tree?))
(require (only-in Fish/Common/game-state fishes? turn? move/c))
(require (only-in Fish/Common/board posn/c))

(provide
 (contract-out
  (struct strategy

    ((place
      ;; place a penguin on an available position, searching from the origin going right, then down
      (-> fishes? posn/c))

     (move
      ;; SANITY CHECK: the color of this player is the color of the first player in the state
      ;; reteturn action, lexicograpphically closest to ORIGIN
      #; #false           ;; -- when the state is final
      #; turn?            ;; -- when a player can skip or move 
      (-> tree? (or/c #false turn?)))))))

;; ---------------------------------------------------------------------------------------------------
(struct strategy (place move) #:transparent)