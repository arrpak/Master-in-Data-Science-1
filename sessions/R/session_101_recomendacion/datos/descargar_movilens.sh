#!/bin/bash

command -v curl >/dev/null 2>&1 || { echo >&2 "I require foo but it's not installed.  Aborting."; exit 1;}

cat LEEME.txt | while read line
do
   curl -L -O $line
   unzip $(basename $line)
   rm $(basename $line)	
done

