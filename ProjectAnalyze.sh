#!/bin/bash

#get everything up to date
git pull

#Puts all uncommited changes in a file changes.log
git status > changes.log

#Puts each line from every file of your project with the tag #TODO into a file todo.log
grep -rl "#TODO" > todo.log

#Checks all haskell files for syntax errors and puts the results into error.log
rm error.log #delete old error.log
find . -name "*.hs" -print0 | xargs -0 -i  ghc {} 2>> error.log

