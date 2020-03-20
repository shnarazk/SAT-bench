# SAT-bench, small utilities about SAT benchmark

- benchm  -- for running a SAT Competition's benchmark
- sat-bench -- running micro benchmark(s)
- satbench2csv

#### Examples

* To compile and run a solver in the repository for a Rust project with options: `-E --rt 0.9`:
```
benchm -B SR19Core -T 300 --options \\-E\ \--rt\ 0.9 --repo ~/Repository/splr
```

The first two backslashes are required to use a string starting with '--' as a value for `--options`.
And, in the argument, a backslash is required before any character that require `escaping`.

* To run a compiled solver with option: `--lucky=false`:
```
benchm -B SR19 -T 100 --options \\--lucky=false --solver ~/.nix-profile/bin/cadical
```

* To rebuild report after some edits:
```
benchm --rereport cadical-20200401 -N 2 -B SC18
```

This will rebuild "cadical-20200401-2-SC18.csv" using files under "cadical-20200401-2-SC18".


## Nix Overlay

https://gitlab.com/shnarazk/SAT/SAT-bench/wikis/home
