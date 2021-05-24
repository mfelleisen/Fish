#lang scribble/manual

@(require "messages.rkt")
@(require (submod Fish/Common/game-state serialize))
@(require (submod Fish/Common/board serialize))
@(require (submod Fish/Common/internal-player serialize))
@(require Fish/Common/board)
@(require Fish/Common/penguin)
@(require Fish/Common/fish)
@(require "shared.rkt")
@(require (only-in racket ~a ~s file->lines))

@(define POSITION 'position)

@(define (include-protocol file)
   (define lines  
     (let* ([lines (file->lines (to-fish file))]
            ; [lines (map (compose smaller tt) lines)]
            [lines (element-join lines "\n")])
       lines))
   (apply verbatim #:indent 4 lines))

@(define (to-fish file) 
   (build-path (getenv "HOME") "Hub" "Fish" "Common" (~a "remote-" file)))

@; -----------------------------------------------------------------------------
@title[#:tag "remote"]{@red{Remote Interactions}}

@author{Matthias Felleisen}

The interaction between Racket player components and the Racket admin framework
is governed by the set of following interaction diagrams. Straight line denote
logical calls, squiggly lines remote messages or calls. Unless otherwise noted,
the diagrams use the same conventions as @secref{local protocol}.

@; -----------------------------------------------------------------------------
@blue{Connecting to the Server}

@include-protocol{protocol-connect.txt}

@bold{Note} The name consist of at least one and at most 12 alphabetical ASCII characters. There is no guarantee that distinct clients sign up with distinct names. It is expected within 10s of opening a connection.

The server accepts TCP connections and represents each as a remote player once the client has submitted a name.  Once
a sufficient number of players have connected to the server and the waiting
period is over, the server signs up these players for a tournament with the
manager and asks it to run a complete tournament.@margin-note*{In real-world
settings, the server may put players that arrive after the waiting period into a
"waiting room."}

@blue{Starting a Tournament}

@include-protocol{protocol-start-tournament.txt}

The protocol for running a tournament remains the same. 

@blue{Terminating a Tournament}

@include-protocol{protocol-end-tournament.txt}

@blue{Starting a Game}

@include-protocol{protocol-launch-game.txt}

@blue{Playing Turns}

@include-protocol{protocol-play-turn.txt}

@; -----------------------------------------------------------------------------

@blue{Method Call Formats}

The logical protocol comprises the six function calls and all we have to do is
describe the JSON format so that logical calls can be realized as remote calls
between components in different languages.

Here is the general format of a function call:
@verbatim[#:indent 4]{
[ String, [@tech{Argument}, ...] ]
}
combining the method name (as below but as a String) with an array of arguments:
@;
@nested[#:style 'inset
   @messages[
 [ [ start         Boolean                                  ] "void"              ]
 [ [ playing-as    @tech{Color}                             ] "void"              ]
 [ [ playing-with  [@tech{Color} (~a " ... ")  @tech{Color}]] "void"          "&" ]
 [ [ setup         @tech{State}                            ] @tech{Position}  "^" ]
 [ [ take-turn     @tech{State} [@tech{Action} (~a " ... ") @tech{Action}] ] @tech{Action} "*, ^"]
 [ [ end           Boolean                                  ] "void"              ]
]

@; -----------------------------------------------------------------------------

@t{Next the referee assigns each player a different penguin-color. The
available @deftech{Colors} are:
@(element-join (map (compose tt ~s) penguin-colors) ", ")

Each player receives @math{6 - N} penguins where @math{N} is the number of
players that participate in the game.}

@verbatim[#:indent 4]{

@deftech{State} is 
  { @(tt (~s (symbol->string PLAYERS))) : @tech{Player*},
    @(tt (~s (symbol->string BOARD))) : @tech{Board} }

@deftech{Player*} is
  [@tech{Player}, ..., @tech{Player}]
INTERPRETATION The array lists all players and specifies the order
in which they take turns. 

@deftech{Player} is
  { @(tt (~s (symbol->string COLOR))) : @tech{Color},
    @(tt (~s (symbol->string SCORE))) : Natural,
    @(tt (~s (symbol->string PLACES))) : [@tech{Position}, ..., @tech{Position}] }
INTERPRETATION The color identifies a player's penguins on the board,  
the score represents how many fish the player has collected so far,
and the last field shows where the player's penguins are located. 

CONSTRAINT All penguins must occupy distinct tiles on the board.}

@verbatim[#:indent 4]{

@deftech{Board-Posn} is 
  { @(tt (~s (symbol->string POSITION))) : @tech{Position},
    @(tt (~s (symbol->string BOARD))) : @tech{Board}}
    
@deftech{Board} is a JSON array of JSON arrays where each element is
either 0 or a number between 1 and @(~a @MAX-FISH).
The size of the board may not exceed a total of 25 spots.
INTERPRETATION A 0 denotes a hole in the board configuration. All other
numbers specify the number of fish displayed on the tile. 

@deftech{Position} is a JSON array that contains two natural numbers:
  [board-row,board-column].
INTERPRETATION The position uses the computer graphics coordinate system
  meaning the Y axis points downwards. The position refers to a tile with at least one fish on it.}

@verbatim[#:indent 4]{@deftech{Action} is
 either
   false
 or
  [ @tech{Position}, @tech{Position} ]
 INTERPRETATION The array describes the opponent's move from the first
 position to the second; if the desired kind of move isn't possible, the
 harness delivers false.}

@; -----------------------------------------------------------------------------

@t{& The array of @tech{Color}s informs a player how many opponents it faces and
the colors that observers use to show them on the game board.}

@t{* The array of @tech{Action}s represents the penguin moves since
the last time the @tt{take-turn} method was called. It is empty if
this is the first call or a player was eliminated since the last
call. A method may use the state to compute a response
functionally xor its own private version of a game tree plus the array
of actions to figure out a response imperatively. The latter allows
caching of tree walks to some extent.}

@t{^ As @tech{State} is used for placing penguins and moving them, it
merely lives up the state specification. That is, it does not satisfy
the post-placement-phase constraints used in past test fests (a
sufficient number of penguins for the given players).}]
@;

@bold{Note} A server or a client may not assume that JSON is well-formed or
valid. 

