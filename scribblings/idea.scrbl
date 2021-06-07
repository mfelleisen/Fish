#lang scribble/manual

@(require "shared.rkt")

@; -----------------------------------------------------------------------------
@title{@blue{The Rules; How to Play}}

The essential rules are spelled out in the paragraph labeled ``Basic Idea.'' In
addition to those, players need to know that
@itemlist[

@item{The referee allocates @math{6 - n} penguins to each player, where @math{n}
is the number of players at the beginning of the game.}

@item{The referee then requests players to place all of their penguins, one at a
time, at the beginning of the game.}

@item{Once all penguins have been placed, the referee calls on players to move
one of their penguins to a different spot until no player can move any
penguins.}

@item{The referee does not ask a player to move a penguin if the current state
of the game does not allow so. Such a player is in a @emph{locked state.}}

@item{A player has only so much time to respond to a referee's request. If a
player does not respond in a timely fashion, the referee disconnects from this
player. All of the player's penguins are removed from the board though the tiles
on which they stand are left in play. Disconnecting a player may thus free up
locked players.}

@item{When a game ends, the players are ranked according to the number of
penguins they own (caught).}

]

@; -----------------------------------------------------------------------------
@bold{How to Play A Game or Several}

Assuming you have installed the repository---see the top entry of the
@tt{README} file---navigate to the @tt{Scripts} directory and run @tt{xgui}:
@;
@verbatim[#:indent 4]{
 $ cd Scripts/
 $ ./xgui n
}
@;
where n is either 1, 2, or 3. This will start a game for @math{n} automated
players, each with a pre-determined name, and one human participant.

When the game is over, the program prints the result to the console and starts
another game with the same participants.

To terminate the program, use @tt{ctrl-c} at the console. 

@emph{Note} At the moment, the game uses a fixed configuration.  The board is
@math{5 x 5}, with all possible places occupied by a tile that carries between 1
and 5 fish. The names of the AI players are fixed. See @emph{todo} in the
@tt{README}. 
