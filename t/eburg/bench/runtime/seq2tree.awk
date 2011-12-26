#!/usr/bin/awk -f

# seq2tree.awk file

BEGIN {
        tmpfile = "/tmp/" ARGV[1] "." int(rand() * 100)
        outfile = ARGV[1] ".erl"
        say("-module('" ARGV[1] "').\n")
        say("-include(\"../../../interface.hrl\").\n")
        say("-compile(export_all).\n\n")
        say("trees() -> [")
}

NF > 0 { # ignore empty basic blocks
        say("\n  ")

        for (i = 1; i <= NF; i++)
                say("#tree{op='" toupper($i) "', kids=[")

        say("#tree{op='S0'}") # the leaf is s0

        for (i = 1; i <= NF; i++)
                say("]}")

        say(",")
}

END {
        say("].\n")
        cmd = "cat " tmpfile " | sed -e 's/,]./\\\
]./' > " outfile
        system(cmd)
        close(tmpfile)
        system("rm -f " tmpfile)
}

function say(str)
{
        printf str >> tmpfile
}

# eof
