#lang scribble/manual

@(require "shared.rkt")

@; -----------------------------------------------------------------------------
@title{@purple{Strategies; How to Tune}}

A strategy for playing Fish consists of two functions:
@;
@itemlist[ #:style 'ordered 

@item{one for helping a player pick a place during the placement phase of the
 game, and} 

@item{another for helping a player move one of its penguin to a new place during
 the proper playing phase.}

]

The implementation comes with an abstract base strategy class in
@filepath{Player/strategy-interface}. This class implements the two 
functions as methods:
@;
@itemlist[ #:style 'ordered 

@item{The @racket[place-penguin] method consumes the current state of the game.
It then traverses the current board in a left-to-right, top-down fashion and
collects all available tiles and their fish counts. The final step is to
@emph{choose} one of those tiles.}

@item{The @racket[move-penguin] method consumes the rules for the current state
of the game. The rules are represented as a tree, with the current state as the
root node with the branches describing all possible actions of the player whose
turn it is. It can thus iterate over all branches and @emph{determine the value}
of the resulting state. The final step is again to @emph{choose} one of these
turns.}

]
@;

As this description suggests, the base strategy relies on two auxiliary methods:
@itemlist[

@item{@racket[(public choose)] for choosing from a list of possible actions (with
values)

The existing definition pre-computes the maximum value of the possibly choices
and all choices with this maximum evaluation. Its @emph{augment} method
determines whether to consider any alternatives. If so, it picks a random one of
those; otherwise it uses a @racket[tie-breaker] argument to pick a choice with
maximal value.}

@item{@racket[(pubment evaluate)] for determining the value of a turn and
the resulting rules.}

]
@;
At a minimum, a concrete strategy must refine these two auxiliary methods; it
may also override both major methods, which may then ignore the auxiliary ones.

One such concrete strategy is @filepath{Player/greedy}:
@;%
@(begin
#reader scribble/comment-reader
(racketblock
;; places penguins on a tile with a maximal fish number
;; moves the penguin to a tile with a maximal fish number (see @racket[tie-breaker])
(define greedy-strategy
  (class base-strategy%
    (super-new)

    (define/augment (choose _the-max _xvalue)
      '[])

    ;; {Turn = [List Posn Posn]}
    (define/override (evaluate trn rules)
      (fish-at (fishes-board (tree-current rules)) (second trn)))))
))
@;%
The @racket[augment] to the @racket[choose] method returns @racket['()] because
there are no alternatives to the maximal-score tiles. The @racket[evaluate]
retrieves the number of fish of the target tile of a turn. Since this second
method receives the rules representing the entire rest of the game, it can
explore what the opponents might do in response---and gain additional strategic
insight; a typical greedy strategy does not employ such a deep look-ahead. 

@bold{Exercise} Take a look at @filepath{Player/random}, a strategy that
takes random actions and, if possible, actions that do not maximize the fish
count. It should be uniformly inferior to the greedy strategy. 

Writing simple strategies similar to the greedy and random ones is
straightforward.

Developing complex, truly AI-ish strategies requires a close look at the common
ontology, specifically @filepath{Common/game-state} and @filepath{Common/rules}.


@; -----------------------------------------------------------------------------
@bold{How to Evaluate a Game With New Strategies}

To find out how your strategy compares to say the greedy default strategy,
navigate to the @tt{Scripts/} directory, set up a configurationn file, and run
@tt{xobserve}: 

@verbatim[#:indent 4]{
$ cd Scripts/ 
$ cat config-s.json 

{ "players" : ["../Player/greedy.rkt",
	       "../Player/random.rkt",
	       "../Player/random.rkt"],
  "rows" : 5,
  "columns" : 4,
  "fish" : 2 }

$ ./xobserve --file config-s.json
(The following players are competing)
(Bob with object:greedy-strategy)
(Aster with object:random-strategy)
(Emma with object:random-strategy)
-----------------------------------
(Bob placed first)
(Aster placed second)
(Emma placed second)
}
@;
Between the fourth and fifth line of output, the program pops up a window, which
is used to display the progress of the game.  Here the greedy player won over
the random player, which is explicitly programmed to be worse than greedy, if
possible, and to make random choices. So the result meets the expectations.

For additional configuration options, see @tt{README} in @tt{Scripts/}.
