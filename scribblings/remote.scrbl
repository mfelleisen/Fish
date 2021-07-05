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
@title[#:tag "sec:remote"]{@red{CO: Remote Interactions}}

A remote player connects to a server via TCP. It is the server's responsibility
to collect connections to remote players, to represent them, and to hand them to
the tournament manager when the conditions are right.

The interaction between remote players and the Racket framework is governed by
the set of following interaction diagrams. Straight line denote logical calls,
squiggly lines remote messages or calls.

The format of the JSON messages is specified below all diagrams in a
single place.

@bold{Organization} The first diagram is about connecting to the server. The
remaining ones specify how the logical interaction diagrams of
@; KEEP ON ONE LINE 
@secref{sec:logic}
@;
are realized via message sending. There is no essential change to the logical
protocol. The revised diagrams merely indicate how calls turn into TCP messages.

Unless otherwise noted, the diagrams use the same conventions as
@; KEEP ON ONE LINE 
@secref{sec:logic}. 

@bold{Note} The diagrams specify far more than is necessary to understand the
connection between a remote player and the server. These details are listed as a
teaching device.

@; -----------------------------------------------------------------------------
@blue{Connecting to the Server}

@include-protocol{protocol-connect.txt}

@bold{Note} There is no guarantee that distinct clients sign up with distinct
names, and there is no need to worry about this for this framework. 

The server accepts TCP connections and represents each as a remote player once
the client has submitted a name.  When a sufficient number of players have
connected to the server and the waiting period is over, the server signs up
these players for a tournament with the manager and asks it to run a complete
tournament.@margin-note*{In real-world settings, the server may put players that
arrive after the waiting period into a "waiting room."}

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

The logical protocol comprises the six function calls. Each of those is realized
as a remote call, with the table below specifying how the call and the return
values are represented as JSON values. 
@;
@verbatim[#:indent 4]{
[ MethodName, [@tech{Argument}, ...] ]
}
combining the method name (as below but as a String) with an array of arguments:
@;
@nested[#:style 'inset
   @messages[
 [ [ start         Boolean                                  ] "void"              ]
 [ [ playing-as    @tech{Color}                             ] "void"              ]
 [ [ playing-with  [@tech{Color} (~a " ... ")  @tech{Color}]] "void"           ]
 [ [ setup         @tech{State}                            ] @tech{Position}   ]
 [ [ take-turn     @tech{State} [@tech{Action} (~a " ... ") @tech{Action}] ] @tech{Action} ]
 [ [ end           Boolean                                  ] "void"              ]
]]

@; -----------------------------------------------------------------------------

@; [#:indent 4]

@verbatim{

@deftech{Color} is one of: @(element-join (map (compose tt ~s) penguin-colors) ", ").
  @bold{Note} The referee assigns them in this order. 

@deftech{State} is 
  { @(tt (~s (symbol->string PLAYERS))) : [@tech{Player}, ..., @tech{Player}], @(tt (~s (symbol->string BOARD))) : @tech{Board} }.

  @bold{Interpretation} The array lists the current state of all players 
  and specifies the order in which they take turns. The board field
  conveys the current state of the board. 

  @bold{Constraint} All players' penguins must occupy mutually
  distinct positions on the board. 

@deftech{Player} is
  { @(tt (~s (symbol->string COLOR))) : @tech{Color}, @(tt (~s (symbol->string SCORE))) : Natural, @(tt (~s (symbol->string PLACES))) : [@tech{Position}, ..., @tech{Position}] }

  @bold{Interpretation} The color identifies a player's penguins on the board, 
  the score represents how many fish the player has collected so far, 
  and the last field shows where the player's penguins are located.
    
@deftech{Board} is
  [ [@tech{Fish#}, ..., @tech{Fish#}], ..., [@tech{Fish#}, ..., @tech{Fish#}] ]
  where @deftech{Fish#} is a Natural between 0 and @(~a @MAX-FISH) (inclusive). 

  @bold{Interpretation} A 0 denotes a hole in the board configuration. 
  All other numbers specify the number of fish on the respective tile.
  The longest row determines the width of the board; rows shorter than 
  that are missing tiles, which is equivalent to 0s. 

@deftech{Position} [Natural, Natural].

  @bold{Interpretation} The position specifies a row and a column.
  The origin is [0,0]. 


@deftech{Action} is one of:
  - false
  - [ @tech{Position}, @tech{Position} ]
 
  @bold{Interpretation} The array describes the opponent's move from the 
  first position to the second.}
