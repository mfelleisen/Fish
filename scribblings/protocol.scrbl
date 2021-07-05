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
@title[#:tag "sec:logic"]{@red{CO: Logical Interactions}}

The interaction between Racket player components and the Racket admin framework
is governed by the set of following @emph{logical} interaction diagrams. The
diagrams should be read in parallel with the player interface specification in
@tt{Common/player-interface.rkt}. The interface consists of a @racket[referee/c]
interface (for the player-referee interactions)  and a @racket[admin-player%/c]
%interface (for the player-manager interactions). 

Keep the following conventions in mind when reading the following diagram. 

@bold{Convention} The @tt{%} parts on the right are interpretive
comments. ~~ A missing return arrow means that method must
successfully return @tt{void}.

The diagrams do not show how the manager and the referees deal with player
failures.

Both the manager and the referees immediately discontinue interacting with any
player that 
@;
@itemlist[

@item{breaks the rules (a ``business logic'' bug)}

@item{raises an exception (a safety bug) @margin-note*{We do not worry about a
player that exploits shared-memory allocation of data representation.}
}

@item{takes too long for a computation (a DoS bug).}

]
@;
While these terminations aren't indicated in the diagram per se, both the
referees and the managers collect the identity of these drop-out players and
return those as part of the result to their caller---which is indicated in the
diagram. 

@; -----------------------------------------------------------------------------
@blue{Starting a Tournament}

@include-protocol{protocol-start-tournament.txt}

The manager is handed any number of players. The ordering determines how these
players are allocated to games and how the referee calls on them. The manager
alerts the players that the tournament is about to start with a @racket[#true]
message. 

@; -----------------------------------------------------------------------------
@blue{Running a Tournament}

@include-protocol{protocol-run-tournament.txt}

For each game, the tournament manager creates a referee and hands over the bunch
of players that compete for just this game. The ordering of the players in this
call determines the order in which the referee calls them back. (On the trace
branch, see @tt{Common/player-interface.rkt} for how this promise is enforced.) 

The referee returns the rankings of the players that finished the game, plus
those players that cheated or failed to respond in time. The manager eliminates
the latter from the tournament. 

@; -----------------------------------------------------------------------------
@blue{Terminating a Tournament}

When the tournament is over, the manager calls the players that finished the
tournament one more time to let them know whether they won or lost.

(The manager drops players that fail to deal with this @racket[end] call. This
can move a winning player into the loser pile if it fails to implement this
callback.) 

@include-protocol{protocol-end-tournament.txt}

#; -----------------------------------------------------------------------------
@blue{Starting a Game}

@include-protocol{protocol-launch-game.txt}

The referee begin calling players in the (reverse) order in which it received them. 

The referee manages a game from the beginning to end. All participating players
are assigned an avatar of a certain color and are informed of this assignment. 

Next the players get told how many other players participate and what their
avatars are. Players do not find out more about their competitors. 

The two pieces of information together tells players in which place they are in
the callback ordering at the beginning of the game.

During the avatar-placement face, the players receive the current state of the
game. At this stage, the only important information is where the competitors
have placed avatars on the board. Players respond with a position to indicate to
the referee whether they wish to place the next avatar.

@; -----------------------------------------------------------------------------
@blue{Playing Turns}

@include-protocol{protocol-play-turn.txt}

Once the setup phase is over, the referee issues @racket[take-turn] callbacks to
the players, in the original ordering. It skips players that have dropped out
and those that in the given state can't move any of their avatars. The second
kind of player is called a @emph{stuck player}; such a player can get unstuck
only if some other player drops out (for reasons spelled out at the top).
