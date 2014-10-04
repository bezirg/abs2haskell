#!/bin/bash

for file in ./*.abs
do
    # all examples contain a main block
    ../dist/build/abs2haskell/abs2haskell --main-is=${file} ${file}
    ghc --make -O -threaded ${file%.*}.hs -o ${file%.*}.out -package-db ../.cabal-sandbox/x86_64-linux-ghc-7.8.3-packages.conf.d -hide-package transformers-0.4.1.0
done
