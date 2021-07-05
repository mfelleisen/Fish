## The Fish Competition 

The repository is a fully functioning game framework. The code is still a work-in-progress. See
TODO at the end. 

The repository is derived from my Fall 2020 Software Development course. I have changed the code
base in many respects to explore some "research-y" topics and reduce the test time.


### TODO

- fix observers, xcall observers, otherwise the referee can crash when the observer crashes.
  - closing the windows should shut down things 
  - use eventspace-thread better? 

- move the `trace` contract material into a branch. 
- develop a `trace` contract for the tournament managers.
  - observer: pause time

### Install

If you wish to inspect the code easily and experiment with it, clone the repo and then install it: 

```
$ git clone git@github.com:mfelleisen/Fish.git
$ cd Fish 
$ raco pkg install 
$ raco doc Fish 
```

This last command will search for the docs of the newly installed "Fish Project"
collection. The results are displayed in a new tab of your default browser. Follow the link
to the project and peruse the documentation, including a version of this README file. 

The package depends on several other repos, including Cameron Moy's `trace` contracts. The
`pkg install` will download and install those.


### The Idea 

["Hey, that's my fish"](https://boardgamegeek.com/boardgame/8203/hey-s-my-fish) (Bambus Spielverlag)
is a board game for players eight and older. It has won several awards and nominations, including the
_Lucca Games Best Family Game_ nominee.

This repository is a framework for programming competitive "Fish" players, a variant of "Hey, that's
my fish". Participants will design automated players that run on their desktops and connect to a
(remote) "Fish" server. This server will run "knock out" tournaments of games consisting of two to
four players each. Any misbehavior of a player---non-responsiveness due to bugs or cheating---results
in immediate termination. So the competition is not just about writing great strategies but also
delivering robust code.

```
 +----------------------------+                           +----------------------------+
 | Client                     |                           | Server                     |
 +----------------------------+                           +----------------------------+
 | player mechanism           |                           | tournament manager         |
 | strategy                   | relies on      relies on  | referees                   |
 | GUI mechanism for people?  |-----------+  +------------| observers                  |
 +----------------------------+           |  |            +----------------------------+
                                          |  |
                                          v  v
                 +---------------------------------------------------------+
                 | the common ontology of Clients and Server               |
                 +---------------------------------------------------------+
                 | player interface and protocols                          |
                 | the rules of the game, expressed as complete game trees |
                 | formulated in terms of game states                      |
                 | which are made up of boards, fish, and penguins         |
                 +---------------------------------------------------------+
```

### The Basic Idea

The "Fish" board is an arrangement of hexagon-shaped tiles, which display some number of fish.
During the start-up phase, players place their penguins on these tiles; a tile cannot hold more than
one penguin. Once the players have placed their penguins, they move their penguins in straight,
uninterrupted lines across the board. The lines are defined by six directions that each tile
represents. When a penguin leaves a tile, the fish on the tile become the player's property and the
tile is removed from the board. Given this context, an _uninterrupted_ line is one that contains no
holes and no tiles occupied by a penguin. The player with the most fish wins the game; the game
permits n-fold ties.

I intend to change aspects of the game to which developers must react with changes to their code
base.

### What You Can Do

Without any coding, you can use this repo to

- observe "AI" games, or
- play interactive games.

Using Racket, you can use this repo to quickly 

- try out new game strategies
- develop different forms of players.

Using any programming language, you can use this repo to

- test remote clients ("AI players") locally 
- stage local and/or remote tournaments. 

The communication between server and clients is TCP-based, using JSON-formatted messages.

### The Common Ontology

For participants to connect to the server, they need to understand the interaction protocol, which
specify the sequence of remote calls and their signatures.  While each message is just a piece JSON
data, the signatures interpret these pieces of data in the context of the game.

Since even a detailed interpretation may leave questions about their meaning with respect to the
rules of the game. the repository's `Common` directory publishes the code that interprets the remote
calls with respect to the server and the sample player in this repository.

### Organization 

The repo consists of the following folders:

| directory   | purpose									      |
| ----------- | ----------------------------------------------------------------------------- |
| Common      | the common ontology, code that is possibly useful for understanding/debugging rules/communication |
| Admin	      | the logical tournament manager, the referee, and a game observer 	      |
| Player      | the logical sample player, both human and automated strategies		      | 
| Remote      | the remote proxy layers for the manager, referee, and player 		      | 
| Scripts     | scripts for running games, tournaments, local and remote; integration `ITests`|
| Lib	      | functionality that should probably exist in Racket's libraries	 	      |
| Resources   | pictures      	   	  	   	    	     			      |
| scribblings | the source of the scribble documentation 				      |

The directories contain additional @tt{README}s so that readers can easily navigate the modules. 

| directory   | purpose									      |
| ----------- | ----------------------------------------------------------------------------- |
| Common | the common ontology: understanding the communication between game server and client player | 
| Admin | the logical tournament manager, referee, and game observer | 
| Scripts | scripts for observing games, playing games, testing remote players, and running tournaments | 
| Player | the logical sample player, both human and automated strategies | 
| Resources | pictures | 
| Lib | functionality that should probably exist in Racket's libraries | 
| Remote | the remote proxy substitutes for the tournament manager, referee, and player | 
| scribblings | the source of the scribble documentation | 
