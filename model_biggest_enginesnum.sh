#!/bin/sh

# Output: Model with biggest number of engines of datafile optd_aircraft.csv

csvsort  -r -d '^' -c nb_engines optd_aircraft.csv | head -2 | csvcut -c model | tail -1
