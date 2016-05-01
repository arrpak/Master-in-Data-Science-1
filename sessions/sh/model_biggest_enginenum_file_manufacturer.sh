#!/bin/sh

# Parameters: $1 Datafile $2 Manufacturer
# Output: Model with biggest number of engines of manufacturer

csvgrep -d '^' -c manufacturer -m $2 $1 | csvsort  -r -c nb_engines | csvcut -c model | head -2 | tail -1
