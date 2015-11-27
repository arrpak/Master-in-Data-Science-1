#!/bin/sh

# Parameters: $1  ziped Datafile 
# Output: Number of columns of Datafile

zless $1 | head -1 | tr "," "\n" | wc -l