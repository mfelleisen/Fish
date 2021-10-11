## Stress Testing 

Two scripts to compare running the program with or without trace contracts: 

1. Stress test with the trace contract turned on: 

```
./xstress-trace
```

2. Stress test without the trace contracts: 


The next one copies the trace-free interface into the place of the interface,
and when the test is finished, does a git-checkout of the original. This is a
bit fragile in case something blows up. The idea solution would be to have
syntax-parameterized modules but alas, we don't. 

```
./xstress-no-trace
```


Both scripts run five one-game tests to make sure things work out,
and then run 101 tournaments with 46 players each. The players use
the gredy strategy, which has the strange effect that all 46 players
end up with the same score and are all winners.
