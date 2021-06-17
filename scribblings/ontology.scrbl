#lang scribble/manual

@(require "shared.rkt")

@; -----------------------------------------------------------------------------
@title{@red{The Common Ontology}}

For participants to connect players to the server, they need to understand the
interaction protocols, both the logical protocol and the remote protocol. The
first specifies the sequence and nature of calls for the various phases of
tournaments and games. The second specifies how to perform these calls over the
network; additionally they explain how to sign up remote players for
tournaments.

Function calls are about sending and receiving values in the chosen programming
language; remote calls are about the exchange of data in a data-exchange
language such as JSON. In either case, the actual meaning of these exchanges
relies on a complete understanding of what each value or piece of data means in
context. 

Since even a detailed explanation may leave questions about the meaning of
values and data, this repository comes with a @tt{Common} directory. The code in
this directory implements all exchanged data and even the rules of the game.
The network messages are then just JSON representations of these values.
@;
@itemlist[

@item{@tt{player-interface} specifies the signature and meaning of the calls
from the tournament managers and the referees to the plug-in players. It relies
on an understanding of the representation of game states and boards.}

@item{@tt{game-state} is a data representation of the state of the game: the
state of the game board and the knowledge of the administrative side about the
players, including the order in which they take turns. In addition to an
understanding of the representation of the state of the game board, it relies on
an understanding of the representation of this knowledge.}

@item{@tt{board} is a data representation of the game board plus positions
within the game board. The @tt{fish} modules represents the tiles.}

@item{@tt{internal-player} represents the knowledge of the administrative
side about an individual player, including the placement of its avatars, the
color of its avatars, and its score. The @tt{penguin} module represents the
penguin avatars.} 

]
Additionally, the @tt{Common/} directory contains a complete implementation of
the rules of the game:
@itemlist[

@item{@tt{rules.rkt} is a data representation of the rules of the game
proper. After the initial placement of penguins, the number of players and the
current state of the board completely determines all possible actions the
players may take. By representing this totality of potential actions, we get an
explicit data representation of the rules. 

Given the current state of the game, the active player may choose from a finite
number of moves of any of its penguins. Each potential move represents one
branch going from the current state of the game to a successor state.  In this
successor state, the playing order is adjusted too. A @emph{final} node in this
tree is one in which none of the players can make a move.

@bold{Note} This rule tree can also be used by internal players to plan
ahead. Since the tree is extremely large, external players will have to use
their own data structure to plan ahead. They may re-build such a tree from the
given states or create an even better planning data structure.}

]

The remaining files are ASCII graphics representing the logical and remote
protocols:
@verbatim[#:indent 4]{
-- protocol-start-tournament.txt
-- protocol-run-tournament.txt
-- protocol-launch-game.txt
-- protocol-play-turn.txt
-- protocol-end-tournament.txt

-- remote-protocol-connect.txt
-- remote-protocol-start-tournament.txt
-- remote-protocol-launch-game.txt
-- remote-protocol-play-turn.txt
-- remote-protocol-end-tournament.txt
}
The next two sections explain the role of these diagrams. 
