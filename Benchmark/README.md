## Stress Testing for the Traced Fish Project

Run 

```
./xstress
```

The outcome is something like this:

```
--- measuring ---
'("16.6" "start:  ../Benchmark/xstress-no-trace.out")
'("81.1" "start:  ../Benchmark/xstress-trace.out")
'("20.0" "start:  ../Benchmark/xstress-trace-no-load.out")
```

These timings are highly inaccurate, because it's just running five bash scripts five times.

- `xstress`
- `xstress-no-trace`
- `xstress-trace-no-load`


[ The names are suggestive. ]

Each of those compiles and then runs the test submodule of

- `xbench`

The final step is to run `measure.rkt` on the `*.out` files, which
collect the timing results. 

### The Benchmark

The benchmark runs a small number of tournaments as many times as `T#`
specifies. The variable is defined at the top:


```
(define T# 10) ;; how many times each tournament is run 
```

The tournaments rank from 5 players to 27 players, so one game to many
games in a row.

The benchmark includes a correctness check so changes to the code base
don't break the measurements (or we discover them quickly).

