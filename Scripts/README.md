## The Scripts for RUnning Fish 

scripts for observing games, playing games, testing remote players, and running tournaments

### Files and directories

- `xobserve players = P` sets up an observable _game_ for `P` greedy players 

- `xplay players = P` sets up a _game_ for one human with `P` greedy opponents 

- `xtest path-to-player players = P port = Q` sets up a _tournament_ for `P` house players plus `path-to-player Q`
- `x1client Q` runs a greedy player on localhost pointed to port `Q`, it logs all calls

- `xclients n p [ip]` points `n` clients to port `p` on `ip`, which defaults to 127.0.0.1
- `xserver p` starts a server that listens on port `p` (w/o house players)

- `config-n.json` is a configuration file that specifies how many house players to include
- `config-s.json` is a configuration file that specifies what kind of house players to include

### Configurations for `xobserve`, `xplay`, and `xtest`

The scripts `xobserve`, `xplay`, and `xtest` take configuration command lines 
(exclusive) or configuration files as inputs.

A command-line configuration has the following shape:

```
fish = F players = P rows = R columns = C ...
```

that is, a word followed by `=` followed by a value.

A file specifies a configuration via a single JSON object:

```
{
  "players" : P,
  "rows" : R,
  "columns" : C,
  "fish" : F
}
```

In most cases, the value is numeric, but some options also allow lists of
paths or Booleans.  The scripts supply default values for missing options.
They also check whether the supplied values are valid,

Here is a table of _all_ options, their defaults, and their legal values:

| option | meaning | default | legal values |
| ------ | ------------------------------------------ | ----- | --------------------------------------- |
| players | number of (additional) players for a game | 2     | (1) a natural in [1,3] (incl.) 		|
| 	  | 	      		   	       	      |	      | (2) a list of paths to strategies 	|
| time-per-turn | the number of secs. the server grants a player to respond to a call | 10 | [1,60]	|
| rows		| the number of rows of the game board 	  	    	       	      | 6  | [[2,9] 	|
| cols		| the number of columns of the game board			      | 6  | [2,9]	|
| fish 		| a fixed number of fish displayed on a tile			      | false | [1,5] 	|
|  		| or a randomly chosen one (`false`)  			      	      |       |  	|
| port		| the port on which server and client communicate		      | 45678 |	(10000,60000) |
| server-wait	| the number of seconds that the server waits to sign up `t-players`  |	30    | natural	      |
| server-tries  | the number of times a server repeats the waiting period	      |  1    | natural       |
| t-players	| the minimum number of players needed to run a tournament	      |  5    | [5,100]	      |
| 		| at the moment this is also the maximum number the server signs up   |	      |		      |

Keep in mind that each script uses only some of them. 

The three scripts look for different sets of options: 

- `xobserve` checks the `rows`, `cols`, `players`, and `fish`.
- `xplay` respects the same options as `xobserve`.
- `xtest` checks for `port`, `server-wait`, `server-tries`, `t-players`, `rows`, `cols`, `fish`, and `time-per-turn`.

### Running a Distributed Tournament: `xserver` and `xclients`

On one machine, launch

```
$ ./xserver port-number
```

and on another one, launch

```
$ ./xclients n port-number host
```

This will sign up `n` players for a tournament where each round
promotoes only the winners of each game. 

### TODO 

- `xplay` and `xobserve` should look for `time-per-turn`.

- `xserver` currently does _not_ respect any options but it can and should use the same ones as `xtest`

- There is currently no way to configure observers or attache them to games and tournaments.  They either exist or don't.

| file | purpose |
|--------------------- | ------- |
