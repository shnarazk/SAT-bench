# SAT-bench, small utilities about SAT benchmark

- benchm  -- for running a SAT Competition's benchmark
- sat-bench -- running micro benchmark(s)
- satbench2csv

#### Examples

* To compile and run a solver in the repository for a Rust project with options: `-E --rt 0.9`:
```
benchm -B SR19Core -T 300 --options \\-E\ \--rt\ 0.9 --repo ~/Repository/splr
```
* To run a compiled solver with option: `--lucky=false`:
```
benchm -B SR19 -T 100 --options \\--lucky=false --solver ~/.nix-profile/bin/cadical
```

## Nix Overlay

https://gitlab.com/shnarazk/SAT/SAT-bench/wikis/home
