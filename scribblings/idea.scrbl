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
@bold{How to Play A Game}

Assuming you have installed the repository---see the top entry of the
@tt{README} file---navigate to the @tt{Scripts} directory:
@verbatim[#:indent 4]{
$ cd Scripts
}

If you just want to observe a game, run @tt{xobserve}:
@;
@verbatim[#:indent 4]{
$ ./xobserve 
}
@;
You can also configure the game with optional arguments. Here is how to set up
all four major parameters:  
@;
@verbatim[#:indent 4]{
$ ./xobserve fish=5 players = 4 row = 2 columns = 3
}
@;
Or if you know JSON, you can set up a JSON configuration file and re-direct the
script to this file:
@;
@verbatim[#:indent 4]{
$ cat config-n.json

{ "players" : 4,
  "rows" : 5,
  "columns" : 4,
  "fish" : 2 }
  
$ ./xobserve --file config-n.json 
}
@;
All players will use the same greedy strategy of occupying tiles with high fish
numbers.

If you wish to play with some number of players, run @tt{xgui}: 
@;
@verbatim[#:indent 4]{
$ cd Scripts/
$ ./xgui n
}
@;
where n is either 1, 2, or 3. This will start a game for @math{n} automated
players, each with a pre-determined name, and one human participant.

When the game is over, the program prints the result to the console. 

To terminate the program, use @tt{ctrl-c} at the console. 

@emph{Limitation} At the moment, @tt{xgui} uses a fixed configuration.  The
board is @math{5 x 5}, with all possible places occupied by a tile that carries
between 1 and 5 fish. The names of the AI players are fixed. See @emph{todo} in
the @tt{README}.
