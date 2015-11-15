#!/bin/sh

# Parameters: $1 datafile $2 number of engines
# Output: Number of models by number of engines

csvgrep -d '^' -c nb_engines -m $2 $1 | tail -n +2 | wc -l
