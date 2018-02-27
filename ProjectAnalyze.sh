#!/bin/bash

#get everything up to date
git pull

#Puts all uncommited changes in a file changes.log
git status > changes.log

#Puts each line from every file of your project with the tag #TODO into a file todo.log
grep -rl "#TODO" > todo.log

#Checks all haskell files for syntax errors and puts the results into error.log
rm error.log #delete old error.log
#Also we compile all haskell files with option -fhpc. Now, when you run your output (in my case .exe) files, it'll automaticelly create .tix file, which you can use to watch your summary of your code
find . -name "*.hs" -print0 | xargs -0 -I '{}'  ghc -fno-code -o '{}'.exe '{}' -fhpc 2>> error.log

#Now, we'll take all existing .tix files and put it in one file hpc.log

rm hpc.log #--//--
find . -name "*.tix" -print0 | xargs -0 -I XXX bash -c 'echo XXX >> hpc.log; hpc report XXX >> hpc.log;'
