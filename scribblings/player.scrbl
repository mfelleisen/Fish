#lang scribble/manual

@(require "shared.rkt")

@; -----------------------------------------------------------------------------
@title{@red{Player; How to Make One}}

A @emph{remote player} is a program that connects to a game server via JSON over
TCP. It may run on the same machine or some other machine. Besides a sign-up
protocol, it must implement just six kinds of pieces of functionality:
@;
@itemlist[

@item{a @emph{start of a tournament} call, which means that the game server has
connected with a sufficiently large number of players and has launched a Fish
tournament;}

@item{a @emph{playing as} call, which informs the remote player that a new game
is about to begin;}

@item{a @emph{playing with} call, which lets the player know how many opponents
participate in a game and which avatars they use;}

@item{a @emph{setup} call, to which the player responds with a placement request
for an avatar;}

@item{a @emph{take turn} call, to which the player responds with a movement
request for one of its avatars on the board;}

@item{an @emph{end of tournament} call, which tells the player whether it won or
lost.} 

]
@;
If a player at any point violates the protocol with a wrong response or delays
the response for more than some given time, the game server will terminate the
communication and kick the player out. Thus, even a winning player could still
be demoted to a ``loser.''

@bold{Suggestion} The remote player implements at least two totally different,
barely related concerns: the game logic and the TCP communication. Separate
those two. @bold{Hint} The game server uses a remote proxy pattern. 

The last three sections describe the interaction protocol---@secref{sec:onto},
@secref{sec:logic}, and @secref{sec:remote}---in detail. 

@; -----------------------------------------------------------------------------
@bold{How to Include a Remote Player in a Full Tournament}

The @tt{Scripts} directory comes with @tt{xtext} for running a tournament with a
single external player that is launched on the same machine:
@;
@verbatim[#:indent 4]{
./xtest path-to-player --file path-to-file.json
or
./xtest path-to-player players = P rows = R ... 
}
@;
Both kinds of configurations must specify a @tt{"players"} attribute.

The @tt{xtest} script configures the server, sets up some number P of
(internal) house players, spawns a separate subprocess using
@tt{path-to-player} as an external player, and then runs a tournament
with those five players.

The @tt{path-to-player} program receives a TCP port number as the
first argument on the command line; it is expected to communicate with
the server on this port via TCP.

The @tt{xtest} script forwards @tt{STDOUT} from the remote player and
the server.

The @tt{x1client} script is a simplistic remote player for testing
purposes.  It is derived from the ``house players'' and displays each
remote call to STDOUT. Sample submissions for other languages are
happily accepted.

Here is what a sample run with these scripts looks like: 
@verbatim[#:indent 4]{
$ cd Scripts/ 
$ ./xtest ./x1client players = 4
5 players are playing a tournament:
 (#(struct:object:player% ...)
  #(struct:object:player% ...)
  #(struct:object:player% ...)
  #(struct:object:player% ...)
  #(struct:object:remote-player% ...))
(pointing one player at 127.0.0.1 on port 45678)

(the remote player was called with (start-of-tournament #t))
(the remote player was called with (playing-as white))
(the remote player was called with (playing-with (red)))
(the remote player was called with (initial #s[...]))
(the remote player was called with (initial #s[...]))
(the remote player was called with (initial #s[...]))
(the remote player was called with (initial #s[...]))
(the remote player was called with (take-turn #s(fishes ...) ()))

(the result produced by the server ((#(struct:object:player% ...)) ()))

(the remote player was called with (take-turn #s(fishes ...) (((0 2) (2 2)))))
(the remote player was called with (take-turn #s(fishes ...) (((0 4) (1 4)))))
(the remote player was called with (take-turn #s(fishes ...) (((1 1) (3 1)))))
(the remote player was called with (take-turn #s(fishes ...) (((1 4) (2 4)))))
(the remote player was called with (take-turn #s(fishes ...) (((2 0) (4 0)))))
(the remote player was called with (take-turn #s(fishes ...) (((2 2) (3 2)))))
(the remote player was called with (take-turn #s(fishes ...) (((2 4) (3 4)))))
(the logging player lost)
all done
}
@;
Notice how the result from the server may show up in the middle of the lines
from the logging player. 

