## The Common Ontology

the common ontology: understanding the communication between game server and client player

The [`Common`](Common/) directory contains the materials that define
the common ontology between external players and the server.

### The APIs

- `player-interface` describes how the service side interacts with players 
- `rules` a data representation for both checking the legality of turns and planning them
- `game-state` a data representation of game states: board, orderig of players, knowledge about players 
- `internal-player` a representation of the knowledge about the players currently in the game 
- `board` a representation of the game board
- `fish` basic constants for the fish tiles
- `penguin` basic constants for the players' avatars 

### The Protocols 

The API assumes the following protocols for the collaboration of the
`Admin` components and `Players`:

- `protocol-start-tournament` ~~ signing up for the tournament
- `protocol-run-tournament` ~~ starting  the round of games 
- `protocol-launch-game` ~~ launching an individual game, set-up phase 
- `protocol-play-turn` ~~ playing a turn during an individual game
- `protocol-end-tournament` ~~ ending the tournament 

A participant has access to this folder because it fully specifies the
rules of engagement.

**Note** The protocol part reveals too much about the internal
organization but none of these revelations should affect safety,
security, or fairness.

| file | purpose |
|--------------------- | ------- |
| [board.rkt](board.rkt) | a board representation for "Fish.com" | 
| [call-in-order.rkt](call-in-order.rkt) | a trace contract that checks the "referee promise" of calling all players | 
| [fish.rkt](fish.rkt) | represents tiles with fish on them | 
| [game-state.rkt](game-state.rkt) | represent the game state for "Fish.com" | 
| [internal-player.rkt](internal-player.rkt) | represents the "ground truth" state of players: | 
| [penguin.rkt](penguin.rkt) | characters that eat fish | 
| [player-interface-error-reporting.rkt](player-interface-error-reporting.rkt) | print the history of how we got to an error in a trace contract | 
| [player-interface-no-load.rkt](player-interface-no-load.rkt) | on the Administrative side, the player has contact with | 
| [player-interface-no-load.rkt~](player-interface-no-load.rkt~) | on the Administrative side, the player has contact with | 
| [player-interface-without-trace.rkt](player-interface-without-trace.rkt) | on the Administrative side, the player has contact with | 
| [player-interface.rkt](player-interface.rkt) | on the Administrative side, the player has contact with | 
| [rules.rkt](rules.rkt) | the game rules, represented complete games as lazy game trees | 
