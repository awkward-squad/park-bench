# Overview

`park-bench` is a quick-and-dirty benchmarking tool for comparing the performance of Haskell functions. Specifically, it
is designed to optimize the workflow in which a programmer makes a small change to a function and wants to measure its
performance impact with as little friction as possible.

# Example usage

Say I am interested in improving the performance of `fib`, which is a function defined in module `Fib` in a local
package called `math-utilities`.

## Step 1: Write the function you'd like to benchmark

First, I'm going to copy the implementation of `fib` to a new top-level definition called `fastfib`, tweak its
implementation, and export both from module `Fib`.

If `fib` was private before, that's ok. We only need to expose it for as long as we are interested in benchmarking.

```haskell
module Fib (fib, fastfib, ...) where
```

## Step 2: Write a standalone `Bench.hs` module with a `main` function

Next, I'm going to write a standalone `Bench.hs`, outside of my local project, which will be compiled to an executable
that runs my benchmark.

```haskell
module Bench where

-- The code in my local package that I want to benchmark
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

Some days, `cabal`/`stack` feel up to the challenge. First I'm going to build my `math-utilities` package, then enter a
shell environment with `math-utilities` in scope for GHC.

```
cabal build && cabal exec -- ghc -O -rtsopts -with-rtsopts=-T Bench.hs
```

```
stack build && stack exec -- ghc -O -rtsopts -with-rtsopts=-T Bench.hs
```

I needed to compile the benchmark with `-rtsopts -with-rtsopts=-T`, otherwise my benchmark will not be able to get RTS
statistics from GHC at runtime.

Alternatively, I could have just compiled the benchmark with `-rtsopts`, but then I'll have to provide `+RTS -T` to the
executable later.

## Step 4: Run the benchmark

If all goes well, I'll have an executable to run.

```
./Bench
```

Or, if I only compiled with `-rtsopts`, but not `-with-rtsopts=-T`,

```
./Bench +RTS -T
```

# Caveat emptor

The statistical analysis performed by `park-bench` is simplistic, written by a novice, and may have bugs. Results should
not necessarily be trusted; please use (or at least compare to) a different tool.
