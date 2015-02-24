# abs2haskell tool [![Build Status](https://travis-ci.org/bezirg/abs2haskell.svg)](https://travis-ci.org/bezirg/abs2haskell)

## Requirements for building the compiler

The compiler itself is written in haskell and distributed as a normal Haskell package.

Therefore to build abs2haskell you need either

1) a recent release of the [Haskell platform](https://www.haskell.org/platform/) (version >= 2014.2.0.0),

2) the GHC compiler accompanied by the Cabal packaging system:

    - GHC compiler (version >=7.8)
    - Cabal package (version >=1.18)
    - `cabal-install` (version >=1.18)

The compiler depends on other community packages/libraries. This program will automatically fetch and install any library dependencies.

## Building and installing the compiler

If you have the above installed then simply run inside the `abs2haskell/` directory:

~~~
sudo make install
~~~

## Running the compiler to generate Haskell code

After installing the compiler, you should have the program `abs2haskell` under your `PATH`.

Examples of running:

~~~
abs2haskell File1.abs File2.abs FileN.abs # compiles 3 abs source files
abs2haskell examples/   # compiles *all* ABS files from given directory
~~~

For usage and extra options, see `abs2haskell --help`.

The compiler will generate ".hs" files for each compiled ABS module. No other runtime system libraries and dependencies will be generated.

## Compiling the generated Haskell code to machine code (so you can run it)

~~~
ghc --make -threaded BenchMaps.hs 

# An ABS program may have multiple main blocks in different modules. 
# So you have to specify with option `-main-is` which module (with a main block)n is your entrypoint

ghc --make -threaded examples/BenchMaps.hs examples/BenchLists.hs -main-is BenchMaps

# The above command will generate an executable named after the main compiled module (here: ./BenchMaps[.exe])
~~~

## Running the final program

~~~
./BenchMaps -O # means run it on 1 core with default optimizations
./BenchMaps -O +RTS -N1 # the same as the above
./BenchMaps -O +RTS -N2 # run it on 2 cores
./BenchMaps -O +RTS -N4 # run it on 4 cores
./BenchMaps -O +RTS -NK # run it on K cores
~~~

## (Optional) Building the Grammar

For the grammar you will need the following programs:

- The [bnf converter](http://bnfc.digitalgrammars.com/)
- The `alex` Haskell lexer
- The `happy` Haskell parser

To install the above dependencies type:

~~~
cabal install 'BNFC>=2.7' alex 'happy>=1.19'
~~~

The grammar lies under `src/ABS.cf`. After your modifications, run:

~~~
make grammar
~~~

to generate the Haskell source files. Then you (re)run `make`, so the abs2haskell compiler can pickup any changes of the grammar.


## (Optional) Building the Documentation

For the documentation you will need heed the following programs:

- The [pandoc documentation converter](http://johnmacfarlane.net/pandoc/)
- The [haddock haskell documentation tool](http://www.haskell.org/haddock/)

To install the above dependencies type:

~~~
cabal install 'pandoc>=13' haddock
~~~

To generate the documentation run:

~~~
make doc
~~~

