
%% Luke Gorrie's favourite profiling macro:
-define(TIME(Tag,Expr),
        (fun() ->
                 %% NOTE: timer:tc/4 does an annoying 'catch' so we
                 %% need to wrap the result in 'ok' to be able to
                 %% detect an unhandled exception.
                 {__TIME, __RESULT} = timer:tc(erlang,apply,[fun() -> {ok,Expr} end,[]]),
                 io:format("time(~s): ~18.3fms ~999p~n", [?MODULE,__TIME/1000, Tag]),
                 case __RESULT of
                     {ok,_} -> element(2, __RESULT);
                     {'EXIT',Error}  -> exit(Error)
                 end
         end)()).

-define(BASENAME, "brew-sequences2").
-define(ALPHA,                    0).
-define(OMEGA,                   65).

bench() ->
    Trees =
        lists:flatmap(
          fun(N) ->
                  Module = list_to_atom(?BASENAME ++ integer_to_list(N)),
                  Module:trees()
          end,
          lists:seq(?ALPHA, ?OMEGA)),
    io:format("~p~n", [length(Trees)]),
    ?TIME(first,  lists:foreach(fun label/1, Trees)),
    ?TIME(second, lists:foreach(fun label/1, Trees)),
    io:format("~p: ~p (~p)~n", [ets:info(burm_state_cache, name),
                                ets:info(burm_state_cache, size),
                                ets:info(burm_state_cache, memory)]),
    io:format("~p: ~p (~p)~n", [ets:info(burm_swdc_cache, name),
                                ets:info(burm_swdc_cache, size),
                                ets:info(burm_swdc_cache, memory)]),
    io:format("~p: ~p (~p)~n", [ets:info(burm_id_cache, name),
                                ets:info(burm_id_cache, size),
                                ets:info(burm_id_cache, memory)]),
    dump().

%%% eof
