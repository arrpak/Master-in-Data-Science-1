#!/bin/sh

# Parameters: $1 Datafile $2 Manufacturer
# Output: All Models with biggest number of engines of manufacturer

maxengines=$(csvgrep -d '^' -c manufacturer -m $2 $1 | csvsort  -r -c nb_engines | csvcut -c nb_engines | head -2 | tail -1)
csvgrep -d '^' -c manufacturer -m $2 $1 | csvgrep -c nb_engines -m $maxengines | csvcut -c model | tail -n +2

# Only one command 

echo
echo 'Now using only one command'
echo 
csvgrep -d '^' -c manufacturer -m $2 $1 | csvgrep -c nb_engines -m $(csvgrep -d '^' -c manufacturer -m $2 $1 | csvsort  -r -c nb_engines | csvcut -c nb_engines | head -2 | tail -1) | csvcut -c model | tail -n +2