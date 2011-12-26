%%% XXX: Only tested interactively!
-module(profile).
-compile(export_all).

%% This takes a while.
profile() ->
    os:cmd("./mk_spec.sh 5"), % gforth5.brl
    fprof:apply(eburg, file, ["gforth5.brl"]), % gforth5.erl, fprof.trace
    fprof:profile(),
    fprof:analyse(dest, []), % fprof.analysis
    file:delete("gforth5.brl"),
    file:delete("gforth5.erl"),
    file:delete("fprof.trace").

%% To use CPROF
%%   1. Uncomment the appropriate lines in eburg.erl
%%   2. Recompile eburg.erl
%%   3. Open an Erlang shell and LOAD ALL EBURG RELATED MODULES MANUALLY
%%   4. Call eburg:file(...)
%%   5. Call cprof:analyse

%% Here's how to dump CPROF's results to a file:
dump() ->
    R = cprof:analyse(),
    {ok, F} = file:open("CPROF.OUT", write),
    io:format(F, "~p~n", [R]),
    file:close(F).

%%% eof
