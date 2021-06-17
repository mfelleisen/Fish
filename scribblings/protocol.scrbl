#lang scribble/manual

@(require "shared.rkt")
@(require (only-in racket file->lines))

@(define (include-protocol file)
   (define lines  
     (let* ([lines (file->lines (to-fish file))]
            ; [lines (map (compose smaller tt) lines)]
            [lines (element-join lines "\n")])
       lines))
   (apply verbatim #:indent 4 lines))

@(define (to-fish file) 
   (build-path (getenv "HOME") "Hub" "Fish" "Common" file))

@; -----------------------------------------------------------------------------
@title[#:tag "local protocol"]{@red{CO: Logical Interactions}}

The interaction between Racket player components and the Racket admin framework
is governed by the set of following interaction diagrams.

@; -----------------------------------------------------------------------------
@blue{Starting a Tournament}

@include-protocol{protocol-start-tournament.txt}

@blue{Running a Tournament}

@include-protocol{protocol-run-tournament.txt}

@blue{Terminating a Tournament}

@include-protocol{protocol-end-tournament.txt}

@blue{Starting a Game}

@include-protocol{protocol-launch-game.txt}

@blue{Playing Turns}

@include-protocol{protocol-play-turn.txt}

Your server does _not_ have to implement the @tt{actions[]}
part of the @tt{take-turn} protocol (always send @tt{[]} instead) but your clients may use the
actions if a server does deliver a non-empty @tt{actions[]} array.

@bold{Convention} The @tt{%} parts on the right are interpretive
comments. ~~ A missing return arrow means that method must
successfully return @tt{void}.

@bold{Termination of Interactions} An interaction between the manager
and the referee on one hand and any player on the other hand is
discontinued if the player 
@;
@itemlist[

@item{breaks the rules (a ``business logic'' bug)}

@item{raises an exception (a safety bug)}

@item{takes too long for a computation (a DoS bug).}

]
@;
We do not worry about a player that exploits shared-memory allocation of data
representation. 

These terminations are not specified in the sequence diagram. 
