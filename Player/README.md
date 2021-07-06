### The Player and Its Strategies 

the logical sample player, both human and automated strategies		    

| file | purpose |
|--------------------- | ------- |
| [game-base-control.rkt](game-base-control.rkt) | a framework for creating a controller: | 
| [game-control.rkt](game-control.rkt) | the actual control classes for a human player mechanism | 
| [greedy.rkt](greedy.rkt) | implement a simple greedy strategy: | 
| [human.rkt](human.rkt) | TODO: eliminate ROW/COLUMN constants from below and allow players to set up dimensions | 
| [player.rkt](player.rkt) | this component implements the mechanics of the player | 
| [random.rkt](random.rkt) | implement a random-pick strategy that avoids the "maximal fish tile" when possible | 
| [strategy-interface.rkt](strategy-interface.rkt) | specify a strategt class with contract | 
