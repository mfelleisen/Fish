## The Maze Competition 

The repository will house a functioning game framework for a variant of the Labyrinth game.
Right now, it contains exploratory code, following BenL's challenges.


### TODO

- split `Exploratory/maze.rkt` into its constituents
- modify the RawBoard representation to deal with Tiles not TileKeys
- design a game state, which is essentially a Player mapping and a spare tile 

### Install

If you wish to inspect the code easily and experiment with it, clone the repo and then install it: 

```
$ git clone git@github.com:mfelleisen/Maze.git
$ cd Fish 
$ raco pkg install 
```

### The Idea 

### The Basic Idea

### What You Can Do

### The Common Ontology

### Organization 

The repo consists of the following folders, with the links pointing to additinal "read me" files:

| directory | purpose |
|--------------------- | ------- |
| [Admin](Admin/README.md) | the logical tournament manager, referee, and game observer (todo) | 
| [Benchmark](Benchmark/README.md) | Run | 
| [Common](Common/README.md) | the common ontology: understanding the communication between game server and client player | 
| [GUI](GUI/README.md) | a game view and its supporting components | 
| [Lib](Lib/README.md) | functionality that should probably exist in Racket's libraries | 
| [Player](Player/README.md) | the logical sample player, both human and automated strategies | 
| [Remote](Remote/README.md) | the remote proxy substitutes for the tournament manager, referee, and player | 
| [Resources](Resources/README.md) | pictures | 
| [Scripts](Scripts/README.md) | scripts for observing games, playing games, testing remote players, and running tournaments | 
| [scribblings](scribblings/README.md) | the source of the scribble documentation | 
