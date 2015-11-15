#!/bin/sh

# Parameters: $1 Datafile
# Output: Model with biggest number of engines

csvsort  -r -d '^' -c nb_engines $1 | head -2 | csvcut -c model | tail -1
