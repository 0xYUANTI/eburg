%%%============================================================================
%%% File     : burm.hrl
%%% Purpose  : BURM support procedures.
%%% Author   : j@svirfneblin.org
%%% License  : BSD
%%% Created  : 2007-08-30 09:58:20 j
%%% Modified : $Id: burm.hrl,v 1.1.1.1 2008/08/07 16:31:33 j Exp $
%%%============================================================================

%%%_. Attributes

-ifdef(DEBUG).
-define(dbg(Fmt, Data), io:format(Fmt, Data)).
-else.
-define(dbg(Fmt, Data), true).
-endif.

-define(INFINITY, 16777216).

%% @todo more detailed type maybe?
%% @todo add this to doc!
-type(tree() :: any()). % user defined

%%%_. State labels

%% Labels are stored with the input tree, in the field accessed by the user
%% provided `state_label' and `state_label_set' functions.
-record(burm_state, {cost :: tuple(),
                     rule :: binary(),
                     id   :: integer()}).


%% ?burm_cost/2 :: (integer(),tree()) -> integer()
%% returns the cost of deriving `Nt' at `Node'.
-define(burm_cost(Nt, Node),
        element(Nt, (state_label(Node))#burm_state.cost)).


%% ?burm_rule/2 :: (integer(),tree()) -> integer()
%% returns the optimal rule for deriving `Nt' at `Node'.
-define(burm_rule(Nt, Node),
        burm_rule(Nt, (state_label(Node))#burm_state.rule)).


%%%_. Labeler
%%% Some notes:
%%%   - State IDs are similar to BURG's state numbers, only generated at
%%%     run-time.
%%%   - Our SWDCs are quite different from the SWDCs in the ``On-Demand
%%%     Tree-Parsing Automata'' paper.

-spec(label/1 :: (tree()) -> tree()).
%% @doc Labels `Tree' (possibly spawning a labeler process first).
label(Tree) ->
    case whereis(burm_labeler) of
        undefined ->
            Self = self(),
            Pid  = spawn_opt(fun() -> burm_labeler(Self) end,
                             [{min_heap_size,10000}]),
            burm_rpc(Pid, Tree);
        Pid ->
            burm_rpc(Pid, Tree)
    end.


-spec(burm_rpc/2 :: (pid(),tree()) -> tree()).
%% @doc Sends `Tree' to `Pid'; re-throws exceptions.
burm_rpc(Pid, Tree) ->
    Pid ! Tree,
    receive
        Err = {burm_exn,_} -> throw(Err);
        Result -> Result
    end.


-spec(burm_labeler/1 :: (pid()) -> no_return()).
%% @doc Sets up the ETS tables used for state caching, then enters an infinite
%% loop.
burm_labeler(Parent) ->
    register(burm_labeler, self()),
    case burm_maybe_load() of
        true ->
            ?dbg("ETS tables loaded~n", []);
        false ->
            ?dbg("creating ETS tables~n", []),
            %% #burm_state{} -> integer()
            ets:new(burm_id_cache, [named_table]),
            %% {atom(), integer(), ..., integer()} -> #burm_state{}
            ets:new(burm_state_cache, [named_table]),
            %% {{atom(), integer(), ..., integer()},
            %%  {integer(), ..., integer()}} -> #burm_state{}
            ets:new(burm_swdc_cache, [named_table])
    end,
    burm_loop(Parent).


-spec(burm_loop/1 :: (pid()) -> no_return()).
%% @doc Receives trees which it labels and sends on to `Pid'.
burm_loop(Pid) ->
    receive
        Tree -> Pid ! (catch burm_label(Tree))
    end,
    burm_loop(Pid).


-spec(burm_label/1 :: (tree()) -> tree()).
%% @doc Returns `Tree' with the `state_label' field set.
burm_label(Tree0) ->
    Op = op_label(Tree0),

    ?dbg("~p: labeling kids~n", [Op]),
    Kids0 = kids(Tree0),
    Kids1 = lists:map(fun burm_label/1, Kids0),
    Tree1 = kids_set(Tree0, Kids1),
    ?dbg("~p: kids labeled~n", [Op]),

    Key0 = list_to_tuple([Op] ++
                         [(state_label(K))#burm_state.id || K <- Kids1]),

    Label =
        case burm_is_tainted(Op) of
            true ->
                Key1 = {Key0,burm_dyncost(Op, Tree1)},
                ?dbg("~p: Key1 is ~p~n", [Op, Key1]),
                case ets:lookup(burm_swdc_cache, Key1) of
                    [] ->
                        L = burm_calc_label(Tree1),
                        ets:insert(burm_swdc_cache, {Key1,L}),
                        ?dbg("~p: swdc calculated and saved: ~p~n~n", [Op, L]),
                        L;
                    [{Key1,L}] ->
                        ?dbg("~p: swdc found: ~p~n~n", [Op, L]),
                        L;
                    _ ->
                        burm_panic({lookup,swdc_cache})
                end;
            false ->
                case ets:lookup(burm_state_cache, Key0) of
                    [] ->
                        L = burm_calc_label(Tree1),
                        ets:insert(burm_state_cache, {Key0,L}),
                        ?dbg("~p: state calculated and saved: ~p~n~n", [Op, L]),
                        L;
                    [{Key0,L}] ->
                        ?dbg("~p: state found: ~p~n~n", [Op, L]),
                        L;
                    _ ->
                        burm_panic({lookup,state_cache})
                end
        end,
    state_label_set(Tree1, Label).


-spec(burm_calc_label/1 :: (tree()) -> #burm_state{}).
%% @doc Returns the full state label for `Node'.
burm_calc_label(Node) ->
    Label = burm_normalize(burm_state(Node)),
    Id    = burm_maybe_reuse_id(Label),
    Label#burm_state{id=Id}.


-spec(burm_normalize/1 :: (#burm_state{}) -> #burm_state{}).
%% @doc Returns `S' with its cost field normalized (we only record the costs of
%% nonterminals relative to each other, not the total costs; this allows us
%% to reuse IDs more frequently).
burm_normalize(S = #burm_state{cost=Cost}) ->
    C0    = tuple_to_list(Cost),
    Delta = lists:min(C0),
    C1    = lists:map(
              fun(C) ->
                      if C =:= ?INFINITY -> C;
                         true -> C - Delta
                      end
              end,
              C0),
    S#burm_state{cost=list_to_tuple(C1)}.


-spec(burm_maybe_reuse_id/1 :: ({tuple(),binary()}) -> integer()).
%% @doc Returns the state number for `Label'.
burm_maybe_reuse_id(Label) ->
    case ets:lookup(burm_id_cache, Label) of
        [] ->
            Id = burm_mk_id(),
            ets:insert(burm_id_cache, {Label,Id}),
            ?dbg("new id: ~p~n", [Id]),
            Id;
        [{Label,Id}] ->
            ?dbg("reusing id: ~p~n", [Id]),
            Id;
        _ -> burm_panic({lookup,id_cache})
    end.


-spec(burm_mk_id/0 :: () -> integer()).
%% @doc Returns a fresh state ID.
burm_mk_id() ->
    N = case get(burm_id) of
            undefined -> 0;
            X -> X+1
        end,
    put(burm_id, N),
    N.


%%%_. Reducer
%%% Note that EBURG generates macro definitions mapping nonterminals to their
%%% internal numbers. Use these when calling `reduce' directly
%%% (e.g. reduce(MyTree, ?soment).

-spec(reduce/2 :: (tree(),integer()) -> any()).
%% @doc Returns the result of executing the user-supplied semantic actions
%% associated with the rules used to label `Tree' in post order.
%% `Nt' indicates the nonterminal the tree is to be reduced to.
reduce(Tree, Nt) ->
    Op   = op_label(Tree),
    Rule = ?burm_rule(Nt, Tree),

    if Rule =:= ?INFINITY -> burm_panic(nomatch);
       true               -> ok
    end,

    Kids0 = kids(Tree),
    Nts   = burm_nts(Rule),
    ?dbg("~p: reducing to ~s~n", [Op, burm_string({nt,Nt})]),
    ?dbg("~p: rule is ~s~n", [Op, burm_string({rule,Rule})]),
    case burm_is_chainrule(Rule) of
        true ->
            ?dbg("~p: chainrule, recursing~n", [Op]),
            KX = reduce(Tree, hd(Nts)), % Nts is singleton list
            ?dbg("~p: firing action~n~n", [Op]),
            burm_action(Tree, [KX], Rule);
        false ->
            ?dbg("~p: baserule, reducing kids~n", [Op]),
            Kids1 = [reduce(T, N) || {T,N} <- lists:zip(Kids0, Nts)],
            ?dbg("~p: firing action~n~n", [Op]),
            burm_action(Tree, Kids1, Rule)
    end.


%%%_. Error handling

-spec(burm_assert/1 :: (bool()) -> ok).
%% @doc Returns `ok' iff its argument is true.
burm_assert(true)  -> ok;
burm_assert(false) -> burm_panic(assert).


-spec(burm_panic/1 :: (_) -> no_return()).
%% @doc Throws a `burm_exn' exception.
burm_panic(Reason) -> throw({burm_exn,Reason}).


-spec(format_error/1 :: (_) -> string()).
%% @doc Returns a descriptive error message for `Reason'.
format_error(load) ->
    "error loading dumped tables";
format_error(dyncost) ->
    "burm_dyncost called with bad operator";
format_error({lookup,Table}) ->
    "bad result: " ++ atom_to_list(Table);
format_error(assert) ->
    "assertion failed";
format_error(noop) ->
    "no such operator";
format_error(nomatch) ->
    "subject tree cannot be matched".


%%%_. Debugging support

-spec(dumpcover/3 :: (tree(),integer(),integer()) -> ok).
%% @doc A custom reducer which prints `Tree's rule cover.
%% `Lvl' is the level of indentation.
dumpcover(Tree, Nt, Lvl) ->
    Rule = ?burm_rule(Nt, Tree),

    if Rule =:= ?INFINITY -> burm_panic(nomatch);
       true               -> ok
    end,

    Kids = kids(Tree),
    Nts  = burm_nts(Rule),
    case burm_is_chainrule(Rule) of
        true ->
            burm_print("~s~n", [burm_string({rule,Rule})], Lvl),
            dumpcover(Tree, hd(Nts), Lvl+1);
        false ->
            burm_print("~s~n", [burm_string({rule,Rule})], Lvl),
            [dumpcover(T, N, Lvl+1) || {T,N} <- lists:zip(Kids, Nts)]
    end,
    ok.


-spec(dumplabels/1 :: (tree()) -> ok).
%% @doc A custom reducer which prints `Tree's state labels.
dumplabels(Tree) ->
    io:put_chars("Operator: Nonterminal using Rule for Cost\n"),
    io:put_chars("=========================================\n"),
    dumplabels(Tree, 0),
    io:put_chars("\n").

dumplabels(Tree, Lvl) ->
    #burm_state{cost=C,rule=R} = state_label(Tree),
    burm_print("~p:~n", [op_label(Tree)], Lvl),
    [burm_print("~s using ``~s'' for ~p~n",
                [burm_string({nt,N}),
                 burm_string({rule,burm_rule(N, R)}),
                 element(N, C)],
                Lvl+1)
     || N <- lists:seq(1, size(C)),
        burm_rule(N, R) =/= ?INFINITY],
    [dumplabels(T, Lvl+2) || T <- kids(Tree)],
    ok.


-spec(burm_print/3 :: (string(),[_],integer()) -> ok).
%% @doc Returns `ok' after `format'ting `Data' and indenting it `N' levels.
burm_print(Fmt, Data, Lvl) ->
    io:format(lists:duplicate(Lvl*2, $\s)++Fmt, Data).


%%%_. Emacs

%%% Local Variables:
%%% allout-layout: t
%%% End:
