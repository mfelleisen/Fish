#lang scribble/manual

@(require "shared.rkt")
@(require Fish/scribblings/overview)
@[define hey-fish-url "https://boardgamegeek.com/boardgame/8203/hey-s-my-fish"]

@; -----------------------------------------------------------------------------

@title[#:tag "Fish Project"]{The Fish Competition}

@author{Matthias Felleisen}

@link[hey-fish-url]{Hey, that's my fish} (Bambus Spielverlag) is a board game
for players eight and older. It has won several awards and nominations,
including @emph{Lucca Games Best Family Game} nomination.

This repository is a framework for programming @emph{Fish} competitions,
specifically software players, that compete in single-game, knock-out
tournaments. The game is a variant of ``Hey, that's my fish;'' use the
human-player graphical interface to get to know it. 

Participants design automated players that run on their desktops and connect to
a (remote) @emph{Fish} server. This server will run "knock out" tournaments of
games consisting of two to four players each. Any misbehavior of a
player---non-responsiveness due to bugs or cheating---results in immediate
termination. So the competition is not just about writing great strategies but
also delivering robust code.

@; ---------------------------------
@centerline[(overview 0.9)]
@; ---------------------------------

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

The @blue{blue section} explains how to play. The @purple{purple section} spell
out how to test new strategies in Racket. The @red{red sections} explain the
interaction protocols, that is, they specify how local or remote AI players
connect to the administrative software, the referee and the tournament
manager. It starts with a section on how to navigate the modules in
@tt{Common/}, which implement the meaning of all interactions.

@;table-of-contents[]

@include-section{rules.scrbl}
@include-section{strategy.scrbl}
@include-section{player.scrbl}
@include-section{tournaments.scrbl}

@include-section{ontology.scrbl}
@include-section{protocol.scrbl}
@include-section{remote.scrbl}

