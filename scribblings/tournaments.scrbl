#lang scribble/manual

@(require "shared.rkt")

@; -----------------------------------------------------------------------------
@title{@black{Tournament; Run One}}

The last step is to run a tournament. All participants should make sure that
their remote player works properly with the provided server. Even then, it is
still possible that a player gets disconnected/terminated due to time
constraints or other problems.

A tournament with 100 players on a single machines takes about 32s.  Running a
distributed variant within a large metro@margin-note*{I know this says nothing
real about how the network traffic is routed.} area takes some 3min. It is not
clear how the server will perform on larger sets of players or in a distributed
setting with large distances.

@; -----------------------------------------------------------------------------
@bold{Run a Distributed Tournament}

The repository comes with scripst for running a distributed tournament.

On one computer (or in one shell), run 
@verbatim[#:indent 4]{
[computer1] $ cd Scripts 
[computer1] $ ./xserver port-number
}
This starts the server on the specified port number. Additional configuration
options may appear on the command line. See @tt{README}. 

On a different computer (or in a different shell), run 
@verbatim[#:indent 4]{
[computer2] $ cd Scripts 
[computer2] $ ./xclients n port-number computer1
}
@;
This will sign up @tt{n} players for a tournament where each round promotoes only the
winner(s) of each game.  The players communicate with the server on the specified
port number running on @tt{computer1}. Omitting @tt{computer1} defaults to local host. 

The clients can of course run on many different computers. They do not have to run
on one or two.

Here is a concrete example:
@verbatim[#:indent 4]{
[computer1] $ ./xserver 12345 t-players = 100
["(#(struct:object:remote-player% ...))","()"]
}

@verbatim[#:indent 4]{
[computer2] $ ./xclients 100 12345
(pointing 100 clients at 127.0.0.1 on port 12345)
all done
}
