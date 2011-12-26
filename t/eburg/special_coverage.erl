
-module(special_coverage).
-import(cover, [compile_beam/1, compile_beam_directory/1, analyse/3]).
-compile(export_all).

%%% FIXME: Check burm.hrl coverage...
main() ->
    %% Cover compile sources
    Ms = compile_beam_directory("../../ebin"),
    Modules0 = [Module || {ok, Module} <- Ms],
    %% Run tests
    eburg:file("special.brl"),
    c:c("special.erl", [{i, "../../include"}, debug_info]),
    {ok, M} = compile_beam("special.beam"),
    Modules = [M|Modules0],
    special:main(),
    %% Analyse internal DB
    Results0 = [{Mod, analyse(Mod, calls, line)} || Mod <- Modules],
    %% Print report
    Results = append([Entries  || {Mod, {ok, Entries}} <- Results0]),
    foreach(fun({Mod, Line}) ->
                    io:format("Not executed: ~p: ~p~n", [Mod, Line])
            end,
            [{Mod, Line} || {{Mod, Line}, Value} <- Results, Value =:= 0]).

%%% eof
