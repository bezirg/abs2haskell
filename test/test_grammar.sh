#!/usr/bin/env bash

i=$((0));  # parsed files
j=$((0));  # succeeded files

if [ $# -eq 0 ]
then
  dirs="grammar compiler/must compiler/could"
else
  dirs="$@"
fi

# WARN
for dir in $dirs
do
    for file in `ls $dir/*.abs`
    do
        i=$((i+1));
        echo "Parsing ${file%.*}"
        ../dist/build/testGrammar/testGrammar ${file} &> ${file%.*}.stderr
        if [ $? -eq 0 ]
        then
            j=$((j+1));
        else
            echo "Parsing error at $file check its .stderr"
        fi
    done
done

echo "$j out of $i files successfully parsed"
