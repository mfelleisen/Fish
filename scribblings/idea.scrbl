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

If you wish to play with some number of players, run @tt{xplay}: 
@;
@verbatim[#:indent 4]{
$ ./xplay
}
@;
This command-line will start a game with a fixed configuration of some mechanical
players plus one GUI for a human player. When the game is over, the program prints
the result to the console.

You can also configure the game with optional arguments:
@;
@verbatim[#:indent 4]{
$ ./xplay players = 3					[1]
... 
$ ./xplay players = 1 row = 4 columns = 3		[2]
... 
$ ./xplay players = 3 row = 4 columns = 3 fish=5	[3]
... 
}
@;
Line 1 overrides the default number of players. Line 2 also overrides the row x
column format. And line 3 tells the script to place 5 fish on every tile, making
the game deterministic and boring. 

If you know JSON, you can set up a JSON configuration file and point the
script to this file:
@;
@verbatim[#:indent 4]{
$ cat config-n.json

{ "players" : 4,
  "rows" : 5,
  "columns" : 4,
  "fish" : 2 }
  
$ ./xplay --file config-n.json 
}
@;

To terminate the program before the game is over, use @tt{ctrl-c} at the console. 
