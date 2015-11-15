#!/bin/sh

# Esto es un comentario
csvsort  -r -d '^' -c nb_engines $1 | head -2 | csvcut -c model | tail -1
