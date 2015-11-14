#!/bin/sh

# Esto es un comentario
csvsort  -r -d '^' -c nb_engines optd_aircraft.csv | head -2 | csvcut -c model | tail -1