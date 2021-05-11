## A Game Inspired by "Hey, that's my fish"

### The Parameters 

- `N` `M` `F`

### The Pieces

- the players: between 2 and 4 players may participate in a game 

- game board: a somewhat rectangular arrangement of N x M hexagonal tiles
  - the tiles are arranged to have their horizontal sides displayed in the up and down direction

```
 create hexagon image (of size s)

       (s,-s)       (2s,-s)
          *------------* 
         /              \
        /                \
 (0,0) *                  * (3s,0)
        \                /
         \              /
          *------------*
       (s,+s)       (2s,+s)
```

- the tiles: the tiles display between 1 and F fish a 

- the penguins: every player receives 6 - #players "penguins" ### Playing 

#### Set Up 

- the youngest player may place one penguin,
- on a tile a single fish
- the players continue to place their penguins in increasing order of age
- re-starting with the youngest 
- until all penguins have been placed


#### Playing

- once the players have placed all their penguins, the youngest player executes a move:
  - the player designates one of his penguins as the active one
  - this action removes the tile on which the penguin rests; the spot on the board is now empty 
  - it becomes a property of the player 
  - the penguin is then moved in a straight line along a sequence of tiles (through the edges only)
  - lines may not cross an empty spot or a tile occupied by any other penguin

- the next oldest player gets the next turn, all the way to the oldest
  which completes a round
  
- this continues until all players hvae placed all their penguins   

- then the next round starts, again with the youngest player 
- a player that cannot move a penguin is skipped for this round

- the game is over when no player can move a penguin during a round

#### Scoring

- the players add up the number of fish on the tiles they
  collected; the player with the largest number of fish wins
  


