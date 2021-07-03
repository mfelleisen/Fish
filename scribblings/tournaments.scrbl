#lang scribble/manual

@(require "shared.rkt")

@; -----------------------------------------------------------------------------
@title{@black{Tournament; Run One}}

@bold{Run a Distributed Tournament}

The repository comes with scripst for running a distributed tournament.

On one computer (or in one shell), run 
@verbatim[#:indent 4]{
[computer1] $ cd Scripts 
[computer1] $ ./xserver port-number
}
This starts the server on the specified port number.

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


