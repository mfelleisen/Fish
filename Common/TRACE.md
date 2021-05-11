### Experiences with Trace Contracts for `Fish`

The purpose of the trace contract on `referee` is to promise client players
that they get to take turns in the specified order---youngest first, oldest
last. It is the responsibility of the `manager` to maintain this order among
the players and to hand them over to each `referee` that manages a game.

      	 [[ I may try to figure out how to set up a trace for this administrative
		     promise too. ]]


The precise promise is based on these assumptions: 

- the referee is handed a list of players;
- the players get their turns to make moves until the referee notices that the game is over; 
- the referee calls the players via their `take-turn` methods;
- but only if the current state of the game allows this particular player to make a move;
- otherwise it is skipped.

In this context, the promise simply says that the players `take-turn` methods
are called, cyclically, in the order in which they were handed to the referee.

#### State of Affairs _Before_ the Injection of Trace 

The code base separated the plugin interface for players from the interface for
the referee. While anybody should be able to create players for the specified
interface (either in Racket or in a different language via a TCP protocol),
nobody needed to know how the referee was organized and how to use it.

#### Injecting the Trace Contract

The development of the trace contract first forced me to re-organize the player
interface contract. The mechanics of `trace/c` set up trace variables that act
like contracts and "collectors of values". These trace variables have to be put
into the appropriate positions within the player contract, while at the same
time, the referee's interface has to say that it accepts a list of players and
produces a list of players:

```
(->i ([sorted-by-age (listof player/c)])
     [ranking (listof (listof player/c))])
```

Each player has to be turned into a trace variable, and the `player/c` contract
has to exist to formulate the `referee` contract. 

The player's `take-turn` contract also has to change because the trace contract
needs to know 

1. the player currently called (`this`)
2. the current state of the game
3. the actions that players have taken since the last callback

to compute the legality of the callback.

   	   [[ Now that I think about it, 2 and 3 are redundant. Perhaps I should work to
	      eliminate the `actions` from the trace predicate. HMPH!  ]]

**Problem 1** The injection of a trace contract makes the `referee/c` and the
 `player/c` contract mutually referential. But our contract system doesn't seem
 to support such contract definitions directly.

Solution. I turned the player contract into a function that consumes trace
variables for the three pieces. Inside the referee contract I can then just
apply the function to the trace variables and construct the desired "ground
contract at the spot where I need it.

Note. I also turned the functional part of the referee contract itself into a
function. That way I could also create player and referee contracts that did
not perform any tracing.

My guess is that this kind of situation will come up on a regular basis. Going
through the function construction may work (almost) all the time. But, it is
awkward.  We should consider additional support for mutually recursive
contracts.

**Problem 2** Formulating the player contract as a function in the player
interface makes no sense to the participants who wish to create AI players.

Solution. I should have realized this from the beginning but the trace
contract is a promise of the referee (creator) to the player (creators).
So the two contracts should live in the same interface module. That way a
player (creator) can see the promise and rely on it. 

Note. Again, this will happen often. Perhaps we need a guideline for this
situation in a manual.

#### Formulating the Trace Contract (see "call-in-order.rkt")

I was surprised how difficult I found it to formulate the `#:fold` predicate
for the trace contract. Here is the essence, in the context of `player-order`,
`state`, `actual-player`, and `actions`: 

```
     (set! player-order (revised-order player-order actions))
     (define current-player* (fishes-players state))
     (define expected-player
       (if (= (length current-player*) (length player-order))
           (first player-order)
           (begin0 actual-player ;; a player dropped out
                   (set! player-order (map iplayer-payload current-player*)))))
     (define okay? (verbose-error-report expected-player actual-player player-order actions state))
     (and okay? (rotate player-order))
```

The `revised-order` function computes the expected order based on
potential required skips (due to blocked players) since the last
`take-turn` call. 

Getting it right was difficult because I hadn't articulated the
promise properly in words and because I had decided to ignore the case
when players drop out due to cheating, network mistakes, etc. But
that's the point of skipping and re-activating players.

The point is, I needed a `#:fold` contract from the beginning. A mere
sequence wasn't enough due to the changes that the player-order could
experience.

#### Mechanics of Trace

The problem with the `#;fold` `trace/c` predicates is that they get
called every single time when a modification to a trace variable
triggers a notification event.

This has several consequences:

- The names of the variables have to be turned into a symbol so that
  an optional `#:name` parameter in the trace predicate can be
  instantiated and thus notify the predicate of the nature of the
  trigger event. 

  Even if the predicated lives in the same module, renaming becomes
  impossible now.

  If the predicate lives in a separate module (file), it gets worse
  because renaming means looking for occurrences of the symbol.

- Next the predicate takes two (essential) arguments:
  - the data description of the event
  - the current value of the accumulator
  If the predicate has to related one trace variable to another, the
  function needs to put these values into an accumulator until all of
  them are available.

  This leads to an awkward programming style. To wit, here is the
  header of the predicate: 

```
(define (call-players-in-given-order-unless-skipped x [player-order '()] #:name which-trace)
  (define player-order0 player-order)
  (case which-trace
    [(player*/c) x]
    [(this/c)    (cons x player-order)]
    [(state/c)   (cons x player-order)]
    [(actions/c)
     (define actions x)
     (match-define (cons state (cons actual-player player-order)) player-order0)
    ...]))
```

Each `case` represents one possible trace variable event:
 
- `player*/c` event happens when the `referee` gets called
- `this/c` is the target object of the method call
- `state/c` is the first argument to the same method call
- `actions/c` is the second argument to the same call

The first case instantiates the accumulator, but sadly, the next two
have to push their arguments also into the accumulator so that they
become available for the third one. The `case` clause for the last
event extracts the three values from the accumulator.

- Note how all three clauses (`this/c`, `state/c`, `actions/c`) can take
  place only for one and only one method call. They need to be grouped
  together, just in case one of them fails due to a behavioral
  contract error. We really need to know that the three pieces came
  from one method call. (This may be guaranteed but it's non-obvious.)

-----------------------------------------------------------------------------

#### Folding and Tracing? Error reporting! 

The program uses two interception points: 

1. the call to `referee` 
2. the callback from the `referee` to each `player`

At 1, the "trace" is re-initialized; at 2, the respective traces are extended. 

The number of players varies over time, because a player may drop out
by raising an exception. So I use `#:fold`.  I also use `#:fold`
because essentially, the three arguments to callbacks tell me whether
I have an acceptable callback ordering; no need to re-traverse the
entire trace every time.

However, I found it really good for debugging to report an error in
terms of the trace, that is, in terms of how the game got into a state
where the referee calls back the wrong player.

So, I re-invented the tracing anyway like this:

```
(struct accu [order this states actions] #:prefab)
#; {Accu = [accu [Listof State] [Listof XPlayer] [Listof [Listof XPlayer]] [Listof [Listof Actions]]]}

(define (call-players-in-given-order-unless-skipped x [aa (accu '() '() '() '())] #:name which-trace)
  (case which-trace
    [(player*/c) (accu (list x) '() '() '[])]
    [(this/c)    (accu+ this)]
    [(state/c)   (accu+ states)]
    [(actions/c) (let-values ([(aa OK) (okay? (accu+ actions))]) (and OK (rotate-player-order aa)))]))
```

The `(accu+ foo)` form cones `x` onto the `foo` field in `aa`. In
essence, the accumulator and its functions are manually
re-implementing the trace data structure. 

The `okay?` function is the workhorse. Its first line shows the
essence:

```
(define (okay? aa)
  (match-define (accu (cons O _) (and this* (cons T _1)) (and state* (cons S _2)) (and actions* (cons A _3))) aa)
```  

Every capital letter denotes the most recent entry into the trace (`O`
for the expected order of players; `T` for `this` aka the `player`
that is being called; `S` for the current state; and `A` for the
actions that happened since last time the player was called (it's
empty if there isn't such a series of actions because it's a new game
or a player dropped out).

#### Error Reporting 

The `okay?` method calls a `verbose-error-reporting` function once it
knows whether the callback is `okay?` (or not): 

```
(verbose-error-report okay? (accu-order aa) this* state* actions*)
```

The function is handed the four pieces in the accumulator. See
[player-interface-error-reporting](player-interface-error-reporting.rkt).
This function spawns a thread for printing the "story" from the
"beginning of the trace" till now. At that point, the error is pretty
obvious and it's possible to say which of the parties is guilty. (Of
course, this may have to do with the "simplistic" trace contract
here.)

**Problem 3** So, the real question is, can we mix traces and #:fold?




