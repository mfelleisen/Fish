### The Player and Its Strategies 

the logical sample player, both human and automated strategies		    

| file | purpose |
|--------------------- | ------- |
| [game-base-control.rkt](game-base-control.rkt) | a framework for creating a controller: | 
| [game-control.rkt](game-control.rkt) | the actual control classes for a human player mechanism | 
| [game-image-snip.rkt](game-image-snip.rkt) | enahnace the image snip with some fields that connect it back to the model | 
| [game-pasteboard.rkt](game-pasteboard.rkt) | a pasteboard editor for displaying the current state of the board and movable avatar snips | 
| [game-view.rkt](game-view.rkt) | a view for a human player mechanism | 
| [greedy.rkt](greedy.rkt) | implement a simple greedy strategy: | 
| [human.rkt](human.rkt) | TODO: eliminate ROW/COLUMN constants from below and allow players to set up dimensions | 
| [player.rkt](player.rkt) | this component implements the mechanics of the player | 
| [random.rkt](random.rkt) | implement a random-pick strategy that avoids the "maximal fish tile" when possible | 
| [strategy-interface.rkt](strategy-interface.rkt) | specify a strategt class with contract | 
