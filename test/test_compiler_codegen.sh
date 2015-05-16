#!/bin/bash

MUST_DIRS=("compiler/must")
i=$((0));

twd=$PWD;

trap 'echo "MUST ERROR: ${BASH_COMMAND} failed, check its .stderr"; exit' ERR
for dir in ${MUST_DIRS}
do
    cd $dir
    # reclean before starting
    rm -f *.stderr *.hs *.hi *.o *.out *.stdout
    for file in ./*.abs
    do
        i=$((i+1));
        echo "${i})Translating ${file%.*} to haskell"
        # all examples should contain a main block
        $twd/../.cabal-sandbox/bin/a2h ${file} 2> ${file%.*}.stderr # no stdout, but redirect TRANSLATE ERRORS
        echo "${i})Compiling ${file%.*} with ghc"
        ghc -w --make -O -threaded ${file%.*}.hs -o ${file%.*}.out -main-is $(basename ${file%.*}) -package-db $twd/../.cabal-sandbox/x86_64-linux-ghc-7.8.4-packages.conf.d  2> ${file%.*}.stderr 1> /dev/null # do not print ghc stdout, but redirect COMPILE ERRORS
        echo "${i})Executing ${file%.*}"
        ${file%.*}.out 1> ${file%.*}.stdout 2> ${file%.*}.stderr  # do not print exe stdout, but redirect RUNTIME ERRORS
        [ $? -eq 0 ] && cat ${file%.*}.stdout | grep -q "False" && exit
    done
    cd -
done
trap - ERR;

# WARN DIRS passed as argv
COULD_DIRS=("compiler/could")
for dir in $COULD_DIRS
do
    cd $dir
    # reclean before starting
    rm -f *.stderr *.hs *.hi *.o *.out
    for file in ./*.abs
    do
        i=$((i+1));
        echo "${i})Translating ${file%.*} to haskell"
        # all examples should contain a main block
        $twd/../.cabal-sandbox/bin/a2h ${file} 2> ${file%.*}.stderr # no stdout, but redirect TRANSLATE ERRORS
        [ $? -ne 0 ] && echo "Translate error at $file check its .stderr" && continue
        echo "${i})Compiling ${file%.*} with ghc"
        ghc -w --make -O -threaded ${file%.*}.hs -o ${file%.*}.out -main-is $(basename ${file%.*}) -package-db $twd/../.cabal-sandbox/x86_64-linux-ghc-7.8.4-packages.conf.d  2> ${file%.*}.stderr 1> /dev/null # do not print ghc stdout, but redirect COMPILE ERRORS
        [ $? -ne 0 ] && echo "Compile error at $file check its .stderr" && continue
        echo "${i})Running ${file%.*}"
        ${file%.*}.out 1> ${file%.*}.stdout 2> ${file%.*}.stderr  # do not print exe stdout, but redirect RUNTIME ERRORS
        if [ $? -ne 0 ] || (cat ${file%.*}.stdout | grep -q "False");
        then echo "Runtime error at $file check its .stderr"
        fi
    done
    cd -
done

# WARN DIRS passed as argv
NEG_DIRS=("compiler/neg")
for dir in $NEG_DIRS
do
    cd $dir
    # reclean before starting
    rm -f *.stderr *.hs *.hi *.o *.out
    for file in ./*.abs
    do
        i=$((i+1));
        { echo "${i})Translating ${file%.*} to haskell" ; \
        $twd/../.cabal-sandbox/bin/a2h ${file} ; \
        echo "${i})Compiling ${file%.*} with ghc" ; \
        ghc -w --make -O -threaded ${file%.*}.hs -o ${file%.*}.out -main-is $(basename ${file%.*}) -package-db $twd/../.cabal-sandbox/x86_64-linux-ghc-7.8.4-packages.conf.d ; }
        echo "${i})Running ${file%.*}"
        ${file%.*}.out 1> ${file%.*}.stdout 2> ${file%.*}.stderr  # do not print exe stdout, but redirect RUNTIME ERRORS
        if [ $? -eq 0 ]; then
            (cat ${file%.*}.stdout | grep -q "False") ||  (cat ${file%.*}.stdout | grep -q "True" && exit)
        fi
    done
    cd -
done
