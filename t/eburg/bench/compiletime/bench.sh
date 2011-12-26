#!/bin/sh

# realtime --- time(1)'s its argument and extracts real time from the results
realtime()
{
    { time -p $1 > /dev/null 2>&1 ; } 2>&1 | awk '/real/ {print $2}'
}

printf "+-------------------------------------------------+\n"
printf "|  States  |  Rules  |  eburg  |  Lines  |  erlc  |\n"
printf "+-------------------------------------------------+\n"

for i in 1 2 3 4 5 6 7 8 9; do
    ./mk_spec.sh $i
    spec=gforth$i.brl
    prog=gforth$i.erl
    byte=gforth$i.beam

    rules=$(grep "<-" $spec | wc -l)
    eburg=$(realtime "../../../bin/eburg $spec")
    lines=$(wc -l $prog | awk '{print $1}')
    erlc=$(realtime "erlc -I../../../include $prog")

    printf "| %8s | %7s | %7s | %7s | %6s |\n" $i $rules $eburg $lines $erlc

    rm -f $spec $prog $byte
done

printf "+-------------------------------------------------+\n"

exit 0

# eof
