#!/bin/sh

# Run the eburg test suite. Results are printed to stdout.

#
# Error handling
#

trap "exit 1"  HUP INT PIPE QUIT TERM
trap "cleanup" EXIT

cleanup()
{
    for t in $tests1 $tests2 $tests3; do
        rm -f $t.erl
        rm -f $t.beam
        rm -f $t.tmp
    done
}

usage()
{
    echo "usage: $0 [-v]\n"
    exit 1
}

#
# Test cases
#

# Output only
tests1="
eburg/jvm
eburg/simple
eburg/special
iburg/sample4
iburg/sample5
mlburg/example2
monoburg/sample
ocamlburg/sample
"

# Full trace
tests2="
eburg/memo1
eburg/memo2
eburg/memo3
eburg/memo4
"

# Error messages
tests3="
eburg/badcode
eburg/badfile
eburg/badlex
eburg/badparse
eburg/badspec
"

#
# Subroutines
#

run()
{
    compile=$1; opts=$2
    shift; shift

    for t in $@; do
        dir=$(echo $t | sed -e 's;\(.*\)/\(.*\);\1;')
        file=$(echo $t | sed -e 's;\(.*\)/\(.*\);\2;')

        herald "$t - Generating BURM... "
        ../bin/eburg $t.brl > $t.tmp
        if $compile; then
            result 0
        else
            result 1
        fi

        if $compile; then
            herald "$t - Compiling BURM... "
            erlc $opts -W0 -I../include -o $dir $t.erl
            result 0
        fi

        herald "$t - Checking output... "
        if $compile; then
            erl -noshell -pa $dir -s $file main -s erlang halt > $t.tmp 2>&1
        fi
        diff $t.out $t.tmp > /dev/null
        result 0
    done
}

tests=0; passed=0

result()
{
    if [ $? -eq $1 ]; then
        tests=$((tests+1))
        passed=$((passed+1))
        herald "ok\n"
    else
        tests=$((tests+1))
        herald "not ok\n"
    fi
}

verbose=0

herald()
{
    if [ $verbose -eq 1 ]; then
        echo -n $1
    fi
}

#
# Do the business
#

while getopts "v" opt; do
    case $opt in
        v) verbose=1;;
        *) usage;;
    esac
done

shift $((OPTIND - 1))
[ $# = 0 ] || usage

run true  ""        $tests1
run true  "-DDEBUG" $tests2 
run false ""        $tests3

echo "$passed out of $tests tests passed"
if [ $passed -eq $tests ]; then
    exit 0
else
    exit 1
fi

# eof
