### The Remote Proxies: The Communication Layer

the remote proxy substitutes for the tournament manager, referee, and player

| file | purpose |
|--------------------- | ------- |
| [basic-constants.rkt](basic-constants.rkt) | the basic constants describing the server specs | 
| [client.rkt](client.rkt) | a client that signs up some players with a server at a given IP address | 
| [manager.rkt](manager.rkt) | a remote manager that connects a single player to a client system, which connects to a server | 
| [player.rkt](player.rkt) | this remote player implements the same interface as the player but conveys its arguments | 
| [server.rkt](server.rkt) | a tournament server that signs up players over TCP and runs a tournament | 
