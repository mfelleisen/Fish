## The Scripts for RUnning Fish 

### Files and directories

- `xgui` starts up a game with one human player and some automated ones 
- `xserver` is the server start-up script
- `xclients` is the client start-up script

### Running a Plain Game

Launch 

```
$ ./xgui n 
```

for some n in 1, 2, or 3. It opens a GUI for dragging penguins 
and placing them, plus an observer. 

### Running a Distributed Tournament or Game

On one machine, launch

```
$ ./xserver port-number
```

and on another one, launch


```
$ ./xclients n port-number [host]
```

This will sign up `n` players for a tournament where each round
promotoes only the winners of each game.

### TODO 

There is currently no way to attach observers to remote
tournaments. It all exists internally but needs to be made
available at the commandline. 

