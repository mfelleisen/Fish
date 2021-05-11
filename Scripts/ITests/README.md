## Integration Tests

### Files and directories

- `../xserver` is the server start-up script
- `../xclients` is the client start-up script
- `xserver-test` is the script that tests the students' servers 
- `xclients-test` is the script that tests the students' clients 

Running the tests will create the file

```
port-starter-file.rktl
```

(see `../Pricate/run-server-client.rkt`).  This file records the last used port
just in case their server doesn't use TCP with reusable ports. The
`x*-test` programs read this file between each re-start. 

### Running tests

```
xclients-test ../xclients 
```

runs the client programs on two scenarios:
- a request for 5 players with outcome [1 0]
- a request for 10 players with outcome [3 0]

```
xserver-test ../xserver 
```

runs the server` program on N scenarios:
- a request for 5 players with outcome [1 0]
- a request for 10 players with outcome [3 0] (with "hiccup" pause in the middle)


The final line of the printed test output uses the same format as the
previous test harnesses. If your test scripts need more, let me know.


