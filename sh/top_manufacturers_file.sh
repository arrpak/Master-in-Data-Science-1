#!/bin/sh

# Parameters: $1 Number of top manufacturers $2  Datafile
# Output: Number of planes and Top manufacturers 

csvcut -d '^' -c manufacturer $2 | tail -n +2 | sort | uniq -c |tail -n +2 | sort -r -n | head -$1