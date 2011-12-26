#!/bin/sh

# mk_spec.sh n

burgmasks="
s[12345678]
s[2345678]
s[345678]
s[45678]
s[5678]
s[678]
s[78]
s8
snone
"

if [ "$1" -eq "" ]; then
    n=9
else
    n=$1
fi

mask=$(echo $burgmasks | head -$((n+1)) | tail -1)
grep -v $mask GFORTH.IN > gforth$n.brl

exit 0

# eof
