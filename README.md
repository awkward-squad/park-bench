# Overview

`park-bench` is a quick-and-dirty benchmarking tool for comparing the performance of Haskell functions. Specifically, it
is designed to optimize the workflow in which a programmer makes a small change to a function and wants to measure its
performance impact with as little friction as possible.

# Example usage

Say I am interested in improving the performance of `fib`, which is a function exposed in a module `Fib` in a local
package called `math-utilities`.

## Step 1: Write the function you'd like to benchmark

First, I'm going to copy the implementation of `fib` to a new top-level definition called `fastfib`, and export it from
module `Fib`.

## Step 2: Write a one-off `Bench.hs` module with a `main` function

Next, I'm going to write `Bench.hs`, which will be compiled to an executable that runs my benchmark.

```haskell
module Bench where

-- The code I want to benchmark
import Fib

-- This library
import ParkBench

main :: IO ()
main =
  benchmark
    [ function "fib" fib 20
    , function "fastfib" fastfib 20
    ]
```

## Step 3: Construct an build environment

Next, I'm going to construct a build environment in which `ghc` can be run at the command-line to compile `Bench.hs`.

Some days, `cabal` feels up to the task. First I'm going to build my package, then use `cabal exec` to enter a shell
environment with `math-utilities` in scope for GHC.

```
> cabal build
> cabal exec -- ghc -O1 -rtsopts -with-rtsopts=-T Bench.hs
```

## Step 4: Run the benchmark

If all goes well, I'll have an executable to run.

```
> ./Bench
```

# Caveats

The statistical analysis performed by `park-bench` is simplistic, written by a novice, and may have bugs. Results should
not necessarily be trusted; please use (or at least compare to) a different tool.
