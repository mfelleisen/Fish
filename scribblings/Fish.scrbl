#lang scribble/manual

@(require "shared.rkt")
@[define hey-fish-url "https://boardgamegeek.com/boardgame/8203/hey-s-my-fish"]

@title[#:tag "Fish Project"]{The Fish Competition}

@author{Matthias Felleisen}

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

@bold{The Basic Idea}

The @emph{Fish} board is an arrangement of hexagon-shaped tiles, which display some number of fish.
During the start-up phase, players place their penguins on these tiles; a tile cannot hold more than
one penguin. Once the players have placed their penguins, they move their penguins in straight,
uninterrupted lines across the board. The lines are defined by the six directions that each tile
represents. When a penguin leaves a tile, the fish on the tile become the player's property and the
tile is removed from the board. Given this context, an @emph{uninterrupted} line is one that contains no
holes and no tiles occupied by a penguin. The player with the most fish wins the game; the game
permits n-fold ties.

The implemented rules differ a bit from those of the published game. 

@bold{What You Can Do}

@blue{Without any coding}, you can use this repo to
@itemlist[
@item{observe "AI" games, or}
@item{play interactive games.}
]

@purple{Using Racket}, you can use this repo to quickly 
@itemlist[
@item{try out new game strategies}
@item{develop different forms of players.}
]

@red{Using any programming language}, you can use this repo to
@itemlist[
@item{test remote clients ("AI players") locally }
@item{stage local and/or remote tournaments.}
]

The communication between server and clients is TCP-based, using JSON-formatted messages.

@; -----------------------------------------------------------------------------
@bold{How to Read the Docs}

The @blue{blue sections} explains how to play. The @purple{purple sections}
spell out how to test new strategies in Racket. The @red{red sections} explain
the remote protocol, that is, they specify how to run local/remote tournaments
with players build in arbitrary programming languages. The last two share the
@black{black section}, on the common ontology between tournament managers and
game referees on one side and AI players on the other. 

@table-of-contents[]

@include-section{idea.scrbl}
@include-section{ontology.scrbl}
@include-section{protocol.scrbl}
@include-section{remote.scrbl}
