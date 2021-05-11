## The Scripts for Running `Fish` Games and Tournaments 

### Files in `bin/`

- `xobserve` runs a game and shows a game observer (good for debugging)
- `xgui` starts up an interactive game with one human player and some automated ones  
- `xserver` is the server start-up script
- `xclients` is the client start-up script

### Running a Plain Game (Debugging Aid)

```
$ ./bin/xobserve [command-line arguments]
```

the command line arguments may optionally specify any of the following "equations":

- fish = F
- players = P
- rows = R
- columns = C
- depth = D

where `F`, `P`, `R`, `C`, and `D` are reasonable fish-tile, player,
row, column, and depth numbers. W/o any arguments it plays some game. 

### Running an Interactive Game (Fun)

Launch 

```
$ ./bin/xgui n 
```

for some n in 1, 2, or 3. It opens a GUI for dragging penguins 
and placing them, plus an observer. 

### Running a Distributed Tournament or Game (Test Fest 10)

On one machine or in one terminal, launch

```
$ ./bin/xserver port-number
```

and on another one or terminal, launch


```
$ ./bin/xclients n port-number [host]
```

This will sign up `n` players for a tournament where each round
promotoes only the winners of each game.

### TODO 

There is currently no way to attach observers to a remote
tournaments. It all exists internally but needs to be made
available at the commandline. 

The warm-up time for clients is 30s per first move. That's a lot and
there should be a way to cut it back. 
