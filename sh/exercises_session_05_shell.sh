#!/bin/sh

# Airline from optd_airlines.csv with most flights

cd ~/Data/opentraveldata
sort -k 14 -rn -t '^' optd_airlines.csv | head -1 | cut -d '^' -f 8,14
sort -k 14 -n  -t '^' optd_airlines.csv | tail -1 | cut -d '^' -f 8,14

# Alliance from optd_airlines with most airlines

cd ~/Data/opentraveldata
cut -d '^' -f 10 optd_airlines.csv | sort | uniq -c  | tail -n +2 | sort -rn | head -1

# Number of column "PASSENGERS" of file T100_SEGMENT_ALL_CARRIER_2015

cd ~/Data/us_dot/traffic
zless T100_SEGMENT_ALL_CARRIER_2015.zip | head -1 | tr "," "\n" > /tmp/columns.csv
numcolumn=$(cat /tmp/columns.csv | wc -l )
seq $numcolumn > /tmp/numcolumns.csv
paste /tmp/columns.csv /tmp/numcolumns.csv | grep "PASSENGERS"
rm /tmp/columns.csv
rm /tmp/numcolumns.csv

# Header and 1st line of file T100_SEGMENT_ALL_CARRIER_2015 transpose to column

cd ~/Data/us_dot/traffic
zless T100_SEGMENT_ALL_CARRIER_2015.zip | head -1 | tr "," "\n" > /tmp/header.csv
zless T100_SEGMENT_ALL_CARRIER_2015.zip | head -2 | tail -1 | sed 's/[,] /. /g' | tr "," "\n" | sed 's/[.] /, /g' > /tmp/1stline.csv
paste -d '^' /tmp/header.csv /tmp/1stline.csv | column -ts '^' | cat
rm /tmp/header.csv
rm /tmp/1stline.csv

# Airline of file T100_SEGMENT_ALL_CARRIER_2015 that flies more segments

cd ~/Data/us_dot/traffic
zless T100_SEGMENT_ALL_CARRIER_2015.zip | tail -n +2 | cut -d ',' -f 11,13  | sort -k 1 | uniq -c | sort -rn | head -1 

# Total Passengers of top 10 by num segment Airline of file T100_SEGMENT_ALL_CARRIER_2014.csv

cd ~/Data/us_dot/traffic
zless T100_SEGMENT_ALL_CARRIER_2014.zip | tail -n +2 | cut -d ',' -f 11  | sort -k 1 | uniq -c | sort -rn > /tmp/topten.csv
for i in {1..10}
do
    topairline=$(head -$i /tmp/topten.csv | tail -1 | cut -d " " -f 4)
    zless T100_SEGMENT_ALL_CARRIER_2014.zip | cut -d ',' -f 5,11 | grep $topairline | cut -d ',' -f 1 > /tmp/passengers.csv
    totpassengers=$(paste -sd+ /tmp/passengers.csv | bc)
    echo $topairline $totpassengers
done
rm /tmp/passengers.csv
rm /tmp/topten.csv

# Extract 7x7 and 3xx patterns from model of file optd_aircraft.csv

cd ~/Data/opentraveldata
cut -d '^' -f 2,3 optd_aircraft.csv | grep 'Boeing' | cut -d '^' -f 2 | grep '^7.7'
cut -d '^' -f 3 optd_aircraft.csv | grep '^[A..Z]3..$'

# Number of airlines with air,airlines or prefix aero of file opt_airlines.csv

cd ~/Data/opentraveldata
cut -d '^' -f 8 optd_airlines.csv| grep -E 'Air|^Aero' | wc -l

# Active Airlines before 1970 of file optd_airlines.csv

cd ~/Data/opentraveldata
cut -d '^' -f3,8 optd_airlines.csv | grep -E '^19[0-6]|^1970' | sort | column -ts '^' | cat 

# url of top 10 by num segment Airline of files T100_SEGMENT_ALL_CARRIER_2014.csv and optd_airlines.csv

touch /tmp/topairlines.csv
rm    /tmp/topairlines.csv
cd ~/Data/us_dot/traffic
zless T100_SEGMENT_ALL_CARRIER_2014.zip | tail -n +2 | cut -d ',' -f 11  | sort -k 1 | uniq -c | sort -rn > /tmp/topten.csv
for i in {1..10}
do
    topairline=$(head -$i /tmp/topten.csv | tail -1 | cut -d " " -f 4  | sed 's/["]//g')
    cd ~/Data/opentraveldata
    cut -d '^' -f 6,8,13 optd_airlines.csv | grep $topairline >> /tmp/topairlines.csv
done
cat /tmp/topairlines.csv | column -ts '^'
rm /tmp/topten.csv
rm /tmp/topairlines.csv

