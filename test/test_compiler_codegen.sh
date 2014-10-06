#!/bin/bash

MUST_DIRS=("compiler/must")
i=$((0));

trap 'echo "MUST ERROR: ${BASH_COMMAND} failed, check its .stderr"; exit' ERR
for dir in ${MUST_DIRS}
do
    # reclean before starting
    rm -f $dir/*.stderr $dir/*.hs $dir/*.hi $dir/*.o $dir/*.out
    for file in ./$dir/*.abs
    do
        i=$((i+1));
        echo "${i})Translating ${file%.*} to haskell"
        # all examples should contain a main block
        ../dist/build/abs2haskell/abs2haskell --main-is=${file} ${file} 2> ${file%.*}.stderr # no stdout, but redirect TRANSLATE ERRORS
        echo "${i})Compiling ${file%.*} with ghc"
        ghc -w --make -O -threaded ${file%.*}.hs -o ${file%.*}.out -package-db ../.cabal-sandbox/x86_64-linux-ghc-7.8.3-packages.conf.d -hide-package transformers-0.4.1.0 2> ${file%.*}.stderr 1> /dev/null # do not print ghc stdout, but redirect COMPILE ERRORS
        echo "${i})Executing ${file%.*}"
        ${file%.*}.out 1> /dev/null 2> ${file%.*}.stderr  # do not print exe stdout, but redirect RUNTIME ERRORS
    done
done
trap - ERR;

# WARN DIRS passed as argv
COULD_DIRS=("compiler/could")
for dir in $COULD_DIRS
do
    # reclean before starting
    rm -f $dir/*.stderr $dir/*.hs $dir/*.hi $dir/*.o $dir/*.out
    for file in ./$dir/*.abs
    do
        i=$((i+1));
        echo "${i})Translating ${file%.*} to haskell"
        # all examples should contain a main block
        ../dist/build/abs2haskell/abs2haskell --main-is=${file} ${file} 2> ${file%.*}.stderr # no stdout, but redirect TRANSLATE ERRORS
        [ $? -ne 0 ] && echo "Translate error at $file check its .stderr" && continue
        echo "${i})Compiling ${file%.*} with ghc"
        ghc -w --make -O -threaded ${file%.*}.hs -o ${file%.*}.out -package-db ../.cabal-sandbox/x86_64-linux-ghc-7.8.3-packages.conf.d -hide-package transformers-0.4.1.0 2> ${file%.*}.stderr 1> /dev/null # do not print ghc stdout, but redirect COMPILE ERRORS
        [ $? -ne 0 ] && echo "Compile error at $file check its .stderr" && continue
        echo "${i})Running ${file%.*}"
        ${file%.*}.out 1> /dev/null 2> ${file%.*}.stderr  # do not print exe stdout, but redirect RUNTIME ERRORS
        [ $? -ne 0 ] && echo "Runtime error at $file check its .stderr"
    done
done
