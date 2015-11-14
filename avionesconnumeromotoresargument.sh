#!/bin/sh

# Esto es un comentario

csvgrep -d '^' -c nb_engines -m $2 $1 | tail -n +2 | wc -l