#!/bin/sh

# Parameters: $1 Datafile $2 Number of Engines
# Output: Number of aircrafs with $2 number of engines of $1 Datafile

csvgrep -d '^' -c nb_engines -m $2 $1 | tail -n +2 | wc -l
