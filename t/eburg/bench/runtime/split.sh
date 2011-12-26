#!/bin/ksh

# gforth sequence file -> .beam files containing Erlang records.
# This takes about an hour and a half on my machine.

csize=500
fsize=`wc -l $1 | awk '{print $1}'`
chunks=$((fsize/csize))

if [ $((fsize%csize)) -ne 0 ]; then
    chunks=$((chunks+1))
fi

i=0
while [ $i -lt $chunks ]; do
    file=$1$i
    tail -n $((fsize - i*csize)) $1 | head -n $csize > $file
    ./seq2tree.awk $file
    rm -f $file
    erlc $file.erl
    rm -f $file.erl
    i=$((i+1))
done

# eof
