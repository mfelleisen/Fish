#lang scribble/manual

@(require "shared.rkt")
@[define hey-fish-url "https://boardgamegeek.com/boardgame/8203/hey-s-my-fish"]


@title{The Fish Competition}

@link[hey-fish-url]{Hey, that's my fish} (Bambus Spielverlag)
is a board game for players eight and older. It has won several awards and nominations, including the
@emph{Lucca Games Best Family Game} nominee.

This repository is a framework for programming competitive @emph{Fish} players, a variant of ``Hey, that's
my fish.'' Participants will design automated players that run on their desktops and connect to a
(remote) @emph{Fish} server. This server will run "knock out" tournaments of games consisting of two to
four players each. Any misbehavior of a player---non-responsiveness due to bugs or cheating---results
in immediate termination. So the competition is not just about writing great strategies but also
delivering robust code.

@verbatim[#:indent 4]{
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
}

@section{The Basic Idea}

The @emph{Fish} board is an arrangement of hexagon-shaped tiles, which display some number of fish.
During the start-up phase, players place their penguins on these tiles; a tile cannot hold more than
one penguin. Once the players have placed their penguins, they move their penguins in straight,
uninterrupted lines across the board. The lines are defined by six directions that each tile
represents. When a penguin leaves a tile, the fish on the tile become the player's property and the
tile is removed from the board. Given this context, an _uninterrupted_ line is one that contains no
holes and no tiles occupied by a penguin. The player with the most fish wins the game; the game
permits n-fold ties.

I intend to change aspects of the game to which developers must react with changes to their code
base.

@section{What You Can Do}

Without any coding, you can use this repo to
@itemlist[
@item{observe "AI" games, or}
@item{play interactive games.}
]

Using Racket, you can use this repo to quickly 
@itemlist[
@item{try out new game strategies}
@item{develop different forms of players.}
]

Using any programming language, you can use this repo to
@itemlist[
@item{test remote clients ("AI players") locally }
@item{stage local and/or remote tournaments.}
]

The communication between server and clients is TCP-based, using JSON-formatted messages.

@section{The Common Ontology}

For participants to connect to the server, they need to understand the interaction protocol, which
specify the sequence of remote calls and their signatures.  While each message is just a piece JSON
data, the signatures interpret these pieces of data in the context of the game.

Since even a detailed interpretation may leave questions about their meaning with respect to the
rules of the game. the repository's `Common` directory publishes the code that interprets the remote
calls with respect to the server and the sample player in this repository.

