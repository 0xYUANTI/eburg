%%%============================================================================
%%% File     : eburg_frontend.erl
%%% Purpose  : Parse, check, and extend input specification.
%%% Author   : j@svirfneblin.org
%%% License  : BSD
%%% Created  : 2007-09-21 11:55:04 j
%%% Modified : $Id: eburg_frontend.erl,v 1.1.1.1 2008/08/07 16:31:33 j Exp $
%%%============================================================================

%%%_. Attributes

-module(eburg_frontend).
-author('j@svirfneblin.org').

-import(eburg_util, [sget/3,
                     die/1, maybe_die/2,
                     c/2, mkcode/3,
                     gsub/3,
                     dumprules/1,
                     uniq/1, enumerate/1, gensym/1, mk_reader/2, next_moe/1,
                     next_pot/1
                    ]).

-export([read/2]).

-include("eburg.hrl").


%%%_. Interface

-spec(read/2 :: (name(),proplist()) -> #eburg{}).
%% @doc Returns the state record from which to generate BURM.
read(File, Opts) ->
    St0 = check(init(File, Opts)),
    ?VERBOSE(begin
                 io:put_chars("Initial rule set:\n"),
                 dumprules(St0#eburg.rules)
             end),
    St1 = reparse(normalize(St0)),
    ?VERBOSE(begin
                 io:put_chars("\n\nFinal rule set:\n"),
                 dumprules(St1#eburg.rules)
             end),
    St1.


%%%_. Initialization
%%% `init' (after processing options) constructs the initial state record:
%%%   - the infile, outfile, module, code, and start fields are set to their
%%%     final values
%%%   - the ops and rules fields are set to their initial values
%%%   - all other fields are set to their default values (undefined in most
%%%     cases)

-spec(init/2 :: (name(),proplist()) -> #eburg{}).
%% @doc Initializes EBURG.
%% Uses the process dictionary.
init(File, Opts) ->
    InDir   = filename:dirname(File),
    OutDir  = proplists:get_value(outdir, Opts, InDir),
    Module  = filename:basename(File, ".brl"),
    InFile  = InDir  ++ "/" ++ Module ++ ".brl",
    OutFile = OutDir ++ "/" ++ Module ++ ".erl",
    Verbose = proplists:get_value(verbose, Opts, false),
    put('$verbose', Verbose),
    (parse(InFile))#eburg{infile  = InFile,
                          outfile = OutFile,
                          module  = Module}.


-spec(parse/1 :: (string()) -> #eburg{}).
%% @doc Returns the internal representation of the specification in `File'.
parse(File) ->
    case file:open(File, [read]) of
        {error,Reason} ->
            die({File,{file,Reason}});
        {ok,Stream} ->
            try
                {Spec,Code} = get_file(Stream),
                Errs = [{File,Line,{code,Mod,Descr}} ||
                           {error,{Line,Mod,Descr}} <-
                               [F || F <- Code, element(1, F) =:= error]],
                maybe_die(Errs, []),
                Reader = mk_reader(fun eburg_lexer:lex/1,
                                   fun eburg_parser:parse/1),
                case Reader(Spec) of
                    {error,{Line,Mod,Descr}} ->
                        die({File,Line,{syntax,Mod,Descr}});
                    St ->
                        St#eburg{code=Code}
                end
             after
                 file:close(Stream)
             end
    end.


%-spec(get_file/1 :: (io_device()) -> {string(), [syntaxTree()]}).
%% @doc Returns the contents of `File', split into declarations & rules and
%% user code.
get_file(Stream) ->
    get_file(Stream, [], 1).

get_file(Stream, Lines, N) ->
    case io:get_line(Stream, '') of
        eof ->
            {Lines, []};
        "Erlang code.\n" ->
            {ok,Forms} = epp_dodger:parse(Stream, N+1), % XXX hrm...
            {Lines,Forms};
        Line ->
            get_file(Stream, Lines ++ Line, N+1)
    end.


%%%_. Basic input validation

-spec(error_info/2 :: (error_descriptor(),#eburg{}) -> error_info()).
-spec(error_info/3 :: (integer(),error_descriptor(),#eburg{}) -> error_info()).
%% @doc Error_info constructor.
error_info(Reason, St)       -> {St#eburg.infile,Reason}.
error_info(Line, Reason, St) -> {St#eburg.infile,Line,Reason}.


-spec(treemap/2 :: (fun(),rhs()|[rhs()]) -> [_]).
%% @doc Returns the results of calling `F' on every nonterminal or operator
%% occuring on `RHS'.
treemap(F, RHS) when is_atom(RHS)     -> F(RHS);
treemap(F, RHS = #pattern{kids=Kids}) -> F(RHS) ++ treemap(F, Kids);
treemap(F, [Kid|Kids])                -> treemap(F, Kid) ++ treemap(F, Kids);
treemap(F, []) when is_function(F, 1) -> [].


-spec(check/1 :: (#eburg{}) -> #eburg{}).
%% @doc Returns its input unchanged if it is found to be valid.
check(St) ->
    Collect = fun(Test, Errs) -> Errs ++ Test(St) end,
    Errs =
        foldl(Collect, [],
              [fun check_code/1,
               fun check_dup/1,
               fun check_start/1,
               fun check_undeclared/1,
               fun check_arity/1]),
    Warns =
        foldl(Collect, [],
              [fun check_unused/1,
               fun check_unreachable/1]),
    maybe_die(Errs, Warns),
    St.


-spec(check_code/1 :: (#eburg{}) -> [error_info()]).
%% @doc Returns syntax errors in dynamic cost and semantic action code.
check_code(St) ->
    Rs      = St#eburg.rules,
    Actions = [{R#rule.line,R#rule.action++"."} ||
                  R <- Rs, R#rule.action =/= ""],
    Dcosts  = [{R#rule.line,R#rule.cost++"."}   ||
                  R <- Rs, is_list(R#rule.cost)],
    Collect = fun(Tag) ->
                      fun({Line,Code}, Errs) ->
                              erl_parse(Line, Tag, Code, Errs, St)
                      end
              end,
    foldl(Collect(action), [], Actions) ++ foldl(Collect(cost), [], Dcosts).

erl_parse(Line, Tag, Code, Errs, St) ->
    Reader = mk_reader(fun erl_scan:string/1, fun erl_parse:parse_exprs/1),
    case Reader(Code) of
        {error,{_Line,Mod,Descr}} ->
            Errs ++ [error_info(Line,{Tag,Mod,Descr},St)];
        _ ->
            Errs
    end.


-spec(check_dup/1 :: (#eburg{}) -> [error_info()]).
%% @doc Returns an error if there are multiply declared operators.
check_dup(St) ->
    case uniq(St#eburg.ops -- uniq(St#eburg.ops)) of
        []   -> [];
        Dups -> [error_info({dup,Dups}, St)]
    end.


-spec(check_start/1 :: (#eburg{}) -> [error_info()]).
%% @doc Returns an error if there are no rules for the start symbol.
check_start(St) ->
    case [R || R <- St#eburg.rules, R#rule.lhs =:= St#eburg.start] of
        [] -> [error_info(start, St)];
        _  -> []
    end.


-spec(check_undeclared/1 :: (#eburg{}) -> [error_info()]).
%% @doc Returns errors for operators which are used but not declared.
check_undeclared(St) ->
        flatmap(fun(R) ->
                        treemap(fun(#pattern{op=Op}) ->
                                        ?IF(member(Op, St#eburg.ops),
                                            [],
                                            [error_info(R#rule.line,
                                                        {undeclared,Op},
                                                        St)]);
                                   (_) -> [] % nt
                                end,
                                R#rule.rhs)
              end,
              St#eburg.rules).


-spec(check_arity/1 :: (#eburg{}) -> [error_info()]).
%% @doc Returns errors for operators used with different arities.
check_arity(St) ->
    Ops    = uniq(St#eburg.ops),
    Used   = treemap(fun getarity/1, [R#rule.rhs || R <- St#eburg.rules]),
    Sorted = [{Op,usort(proplists:get_all_values(Op, Used))} || Op <- Ops],
    [error_info({arity,Op,As}, St) || {Op,As} <- Sorted, length(As) > 1].

getarity(#pattern{op=Op,kids=Kids}) -> [{Op,length(Kids)}];
getarity(_) -> [].


-spec(check_unused/1 :: (#eburg{}) -> [error_info]).
%% @doc Returns warnings for any unused operators.
check_unused(St) ->
    Used   = treemap(fun getop/1, [R#rule.rhs || R <- St#eburg.rules]),
    Unused = uniq(St#eburg.ops) -- Used,
    [error_info({unused,Op}, St) || Op <- Unused].

getop(#pattern{op=Op}) -> [Op];
getop(_)               -> [].


-spec(check_unreachable/1 :: (#eburg{}) -> [error_info()]).
%% @doc Checks for unreachable nonterminals by traversing the ruleset from
%% LHS-nonterm to RHS-nonterm (the inverse of the reduces-to relation).
%% E.g.:
%%   start <- FOO(bar)
%%   bar   <- baz
%% Here, baz is reachable from start via bar.
check_unreachable(St) ->
    Rs     = St#eburg.rules,
    Start  = St#eburg.start,
    Seen   = reach([R || R <- Rs, R#rule.lhs =:= Start], [Start], Rs),
    Unseen = uniq([R#rule.lhs || R <- St#eburg.rules]) -- Seen,
    [error_info({unreachable,Nt}, St) || Nt <- Unseen].

reach([#rule{rhs=RHS}|Rs], Seen, Rules) ->
    Nts = treemap(fun getnt/1, RHS),
    New = [R || R <- Rules, member(R#rule.lhs, Nts -- Seen)],
    reach(Rs ++ New, Seen ++ Nts, Rules);
reach([], Seen, _) -> Seen.

getnt(Nt) when is_atom(Nt) -> [Nt];
getnt(_)                   -> [].


%%%_. Pseudo variables

-define(PVARS, 64).

-spec(pvar/1 :: (a | 1..?PVARS) -> string()).
%% @doc Pseudo variable constructor.
pvar(a) -> "'$a'";
pvar(N) when N >= 1,N =< ?PVARS -> concat(["'$", N, "'"]).


-spec(known_pvars/0 :: () -> [integer()]).
%% @doc Used to iterate over all pvars.
known_pvars() -> seq(1, ?PVARS).


-spec(expand_pvar/2 :: (string(),a|1..9) -> string()).
%% @doc Expand pseudo variable `Pvar' in `Code'.
expand_pvar(Pvar, Code0) ->
    {Old,New}    = stab(Pvar),
    {ok,Code1,_} = gsub(Code0, Old, New),
    Code1.

%% Dynamic cost code is included in `burm_state', `closure_*', and
%% `burm_dyncost'. Semantic action code is included in `burm_action'.
%% These must ensure that the following expansions are valid.
%%          Pseudo var  Actual var
stab(a) -> {pvar(a)   , "BurmNode"};
stab(N) -> {pvar(N)   , c("lists:nth(~p, BurmReducedKids)", [N])}.


%%%_. Normalization
%%% A rule is in normal form if it is either a chainrule or a baserule whose
%%% RHS operator's children are all nonterminals (i.e., rule normalization
%%% eliminates nested patterns).
%%%
%%% The rule set
%%%
%%%     foo <- BAR(baz, QUUX(FROB(blorp), snarfle)) : ['$1', '$2', '$3'];
%%%
%%% is not in normal form, since QUUX(...) is not a nonterminal.
%%% A single normalization step yields:
%%%
%%%     foo <- BAR(baz, burm_nt0) : ['$1', element(1, '$2'), element(2, '$2')];
%%%     burm_nt0 <- QUUX(FROB(blorp), snarfle) : {'$1', '$2'};
%%%
%%% Notice how both the rule's RHS and its semantic action have to be
%%% rewritten. A second normalization step gives the final rule set:
%%%
%%%     foo <- BAR(baz, burm_nt0) : ['$1', element(1, '$2'), element(2, '$2')];
%%%     burm_nt0 <- QUUX(burm_nt1, snarfle) : {'$1', '$2'};
%%%     burm_nt1 <- FROB(blorp) : '$1';

-spec(count_nts/1 :: (rhs()) -> integer()).
%% @doc Returns the number of nonterminals occuring in `RHS'.
count_nts(Nt) when is_atom(Nt) -> 1;
count_nts(#pattern{kids=Kids}) -> lists:sum(map(fun count_nts/1, Kids)).


-spec(normalize/1 :: (#eburg{}) -> #eburg{}).
%% @doc Sets up the `rule_cache', then initiates normalization.
normalize(St) ->
    %% for interactive development:
    ?IF(member(rule_cache, ets:all()), ets:delete(rule_cache), ok),
    %% {RHS :: #pattern{}, LHS :: atom()}
    ets:new(rule_cache, [named_table]),
    St#eburg{rules=do_normalize(St#eburg.rules)}.


-spec(do_normalize/1 :: ([#rule{}]) -> [#rule{}]).
%% @doc Iterates over `Rs' until it becomes stable.
do_normalize(Rs0) ->
    {Changes, Rs1} = normalize_loop(Rs0),
    if Changes -> do_normalize(Rs1);
       true    -> Rs1
    end.


-spec(normalize_loop/1 :: ([#rule{}]) -> [#rule{}]).
%% @doc Loops over `Rs' calling `normalize_rule' as necessary.
%% We try to keep the rules in the order in which they appeared in the
%% input file, with new rules spliced in after their parent rules
%% (ordered by child replaced).
normalize_loop(Rs) ->
    normalize_loop(Rs, [], false).

normalize_loop([], Rules, Flag) ->
    {Flag, Rules};
normalize_loop([R|Rs], Rules, Flag) ->
    case is_normalized(R) of
        true  -> normalize_loop(Rs, Rules ++ [R], Flag);
        false -> normalize_loop(Rs, Rules ++ normalize_rule(R), true)
    end.


-spec(is_normalized/1 :: (#rule{}) -> bool()).
%% @doc Returns `true' if `R' is in normal form.
is_normalized(#rule{rhs=Nt}) when is_atom(Nt) ->
    true;
is_normalized(#rule{rhs=#pattern{kids=Kids}}) ->
    all(fun erlang:is_atom/1, Kids).


-spec(normalize_rule/1 :: (#rule{}) -> [#rule{}]).
%% @doc Returns `R' in normal form plus new rules for the sub-patterns on
%% `R's RHS. `R' is assumed to _not_ be in normal form already.
normalize_rule(R = #rule{rhs=#pattern{kids=Kids0}}) ->
    {Kids1,Rs} = rewrite_pattern(Kids0),
    Action     = rewrite_action(R),
    [R#rule{rhs=(R#rule.rhs)#pattern{kids=Kids1},action=Action}|Rs].


-spec(rewrite_pattern/1 :: ([rhs()]) -> {[atom()],[#rule{}]}).
%% @doc Returns the new list of kids (now atomic) and a list of new rules
%% (which may have to be normalized further). Also enters the new rules
%% into the `rule_cache' table (in fact, it only puts each new rules'
%% LHS, indexed by original sub-pattern, into the ETS table; this rule
%% may be further normalized later but its LHS will remain the same).
%% New rules have zero cost.
rewrite_pattern(Kids) ->
    rewrite_pattern(Kids, [], []).

rewrite_pattern([], Kids, Rules) ->
    {reverse(Kids),Rules};
rewrite_pattern([K|Ks], Kids, Rules) when is_atom(K) ->
    rewrite_pattern(Ks, [K|Kids], Rules);
rewrite_pattern([K|Ks], Kids, Rules) when is_record(K, pattern)->
    case ets:lookup(rule_cache, K) of
        [{_, Nt}] ->
            rewrite_pattern(Ks, [Nt|Kids], Rules);
        [] ->
            Nt = gensym(burm_nt),
            ets:insert(rule_cache, {K, Nt}),
            R = #rule{lhs=Nt,rhs=K,action=gen_action(count_nts(K))},
            rewrite_pattern(Ks, [Nt|Kids], Rules ++ [R])
    end.


-spec(gen_action/1 :: (integer()) -> string()).
%% @doc Returns the semantic action code for a generated rule with `N'
%% children.
gen_action(0) -> "ok";
gen_action(1) -> pvar(1);
gen_action(N) -> mkcode(tuple, "~s", [pvar(X) || X <- seq(1, N)]).


-spec(rewrite_action/1 :: (#rule{}) -> #rule{}).
%% @doc Returns `R' with pvar references in its semantic action code rewritten
%% to match the normalized rule set.
rewrite_action(#rule{rhs=#pattern{kids=Kids}, action=A}) ->
    Temp = foldl(fun temp_action/2, A, get_pvar_mapping(Kids)),
    foldl(fun action/2, Temp, known_pvars()).

temp_action({Old,New}, A0) when is_integer(New) ->
    {ok,A1,_} = gsub(A0, pvar(Old), burm_temp(New)), A1;
temp_action({Old, {elt,Pos,New}}, A0) ->
    {ok,A1,_} = gsub(A0, pvar(Old), burm_temp(Pos, New)), A1.

action(N, A0) -> {ok,A1,_} = gsub(A0, burm_temp(N), pvar(N)), A1.

burm_temp(N)      -> c("burm_temp~p", [N]).
burm_temp(Elt, N) -> c("element(~p, burm_temp~p)", [Elt, N]).


-spec(get_pvar_mapping/1 :: ([rhs()]) -> [{integer(),integer()}
                                          | {integer(),
                                             {elt,integer(),integer()}}]).
%% @doc Returns the mapping of old pvars to new (post-normalization) pvars.
%% A value of {N, M} means that the old pvar number N maps to the new pvar M,
%% whereas a value of {N, {elt, Pos, M}} means that the old pvar N maps to
%% the Pos-th element of the new pvar M.
get_pvar_mapping(Kids) ->
    get_pvar_mapping(Kids, 1, 1).

get_pvar_mapping([K|Ks], N, M) when is_atom(K) ->
    [{N,M}|get_pvar_mapping(Ks, N+1, M+1)];
get_pvar_mapping([K|Ks], N0, M) when is_record(K, pattern) ->
    X = count_nts(K),
    if X=:=0 -> get_pvar_mapping(Ks, N0, M+1); % skip
       X=:=1 -> [{N0,M}|get_pvar_mapping(Ks, N0+1, M+1)];
       %% new pvar M returns old pvars N..N+X-1
       true -> Map = [{N1,{elt,Pos,M}} ||
                         {N1,Pos} <- zip(seq(N0, N0+X-1), seq(1, X))],
               Map ++ get_pvar_mapping(Ks, N0+X, M+1)
    end;
get_pvar_mapping([], _N, _M) ->
    [].


%%%_. Reparsing

-spec(reparse/1 :: (#eburg{}) -> #eburg{}).
%% @doc Finalizes the input specification and adds some derived data structures
%% to the state record. Assumes `St' has been `check'ed.
reparse(St) ->
    Update = fun(Fun, StAcc) -> Fun(StAcc) end,
    foldl(Update, St,
          [fun reparse_nts/1, % order matters
           fun reparse_ops/1,
           fun reparse_dyncosts/1,
           fun reparse_actions/1,
           fun reparse_rules/1,
           fun reparse_taints/1,
           fun reparse_arities/1,
           fun reparse_packmap/1
          ]).


-spec(reparse_nts/1 :: (#eburg{}) -> #eburg{}).
%% @doc Adds a list of all known nonterminals and a mapping of nonterminals to
%% INNs to the state record.
reparse_nts(St) ->
    Nts = uniq([R#rule.lhs || R <- St#eburg.rules]),
    St#eburg{nts=Nts,inns=enumerate(Nts)}.


-spec(reparse_ops/1 :: (#eburg{}) -> #eburg{}).
%% @doc Drops unused operators.
reparse_ops(St) -> St#eburg{ops=uniq(St#eburg.ops)}.


-spec(reparse_rules/1 :: (#eburg{}) -> #eburg{}).
%% @doc Attaches IRNs to rules and populates the state record's crules and
%% and brules fields.
reparse_rules(St) ->
    Rules  = [R#rule{number=N} || {R,N} <- enumerate(St#eburg.rules)],
    Crules = [{Nt,[R || R <- Rules,  R#rule.rhs             =:= Nt]}
              || Nt <- St#eburg.nts],
    Brules = [{Op,[R || R <- Rules, (R#rule.rhs)#pattern.op =:= Op]}
              || Op <- St#eburg.ops],
    St#eburg{rules=Rules,crules=Crules,brules=Brules}.


-spec(reparse_dyncosts/1 :: (#eburg{}) -> #eburg{}).
%% @doc Finalizes dynamic costs by wrapping them into some error checking code
%% and expanding pseudo variables.
reparse_dyncosts(St) ->
    Rs0 = St#eburg.rules,
    Rs1 = map(fun maybe_wrap_cost/1, Rs0),
    Rs2 = map(fun expand_pvars_cost/1, Rs1),
    St#eburg{rules=Rs2}.

maybe_wrap_cost(R=#rule{cost=C}) when is_list(C) -> R#rule{cost=wrap_cost(C)};
maybe_wrap_cost(R=#rule{cost=C}) when is_integer(C) -> R.

wrap_cost(Cost) ->
    BurmTemp = gensym("BurmTemp"),
    c("
begin
    ~s = (catch
              begin
                  ~s
              end),
    case is_integer(~s) of
        true  -> ~s;
        false -> ~p
    end
end", [BurmTemp, Cost, BurmTemp, BurmTemp, ?INFINITY]).

expand_pvars_cost(R = #rule{cost=C}) when is_list(C) ->
    R#rule{cost=expand_pvar(a, C)};
expand_pvars_cost(R = #rule{cost=C}) when is_integer(C) ->
    R.


-spec(reparse_actions/1 :: (#eburg{}) -> #eburg{}).
%% @doc Expand pseudo variables in semantic action code.
reparse_actions(St) ->
    Rs0 = map(fun expand_pvars_action/1, St#eburg.rules),
    Rs1 = map(fun add_default_action/1, Rs0),
    St#eburg{rules=Rs1}.

expand_pvars_action(R = #rule{action=A}) ->
    R#rule{action=foldl(fun expand_pvar/2, A, known_pvars() ++ [a])}.

add_default_action(R = #rule{action=""}) -> R#rule{action="ok"};
add_default_action(R)                    -> R.


-spec(reparse_taints/1 :: (#eburg{}) -> #eburg{}).
%% @doc Computes the taint list for each operator by traversing the ruleset
%% from RHS-operator to LHS-nonterm (i.e. it checks the chainrule closure for
%% each base rule's LHS).
reparse_taints(St) ->
    Curried = fun(Op) -> getdyncosts(Op, St) end,
    St#eburg{taints=[{Op,Curried(Op)} || Op <- St#eburg.ops]}.

getdyncosts(Op, St) ->
    getdyncosts(sget(Op, brules, St), [], St#eburg.rules, []).

getdyncosts([#rule{lhs=Nt,cost=Cost}|Rs], Seen, Rules, Acc0) ->
    Acc1 = ?IF(is_list(Cost), [Cost|Acc0], Acc0),
    New  = ?IF(member(Nt, Seen), [], [R || R <- Rules, R#rule.rhs =:= Nt]),
    getdyncosts(Rs ++ New, [Nt|Seen], Rules, Acc1);
getdyncosts([], _, _, Acc) -> reverse(Acc).


-spec(reparse_arities/1 :: (#eburg{}) -> #eburg{}).
%% @doc Builds the operator arity table.
reparse_arities(St) ->
    Curried = fun(Op) -> arity(Op, St) end,
    St#eburg{arities=[{Op,Curried(Op)} || Op <- St#eburg.ops]}.

arity(Op, St) ->
    [#rule{rhs=#pattern{op=Op,kids=Kids}}|_] = sget(Op, brules, St),
    length(Kids).


-spec(reparse_packmap/1 :: (#eburg{}) -> #eburg{}).
%% @doc Constructs the table needed for rule packing and the rule vector size.
reparse_packmap(St) ->
    Curried = fun(Nt) -> rmap_entry(Nt, St) end,
    Pmap0   = map(Curried, St#eburg.nts),
    Rvsize  = next_moe(lists:sum([Bs || {_,Bs,_} <- Pmap0])),
    Pmap1   = pad_last(Pmap0, Rvsize),
    St#eburg{rvsize=Rvsize,pmap=Pmap1}.

rmap_entry(Nt, St) ->
    Rs = [R || R <- St#eburg.rules, R#rule.lhs =:= Nt],
    {Nt,next_pot(length(Rs)),
     [{?INFINITY, 0}] % a zero PRN indicates `no match'
     ++ enumerate([R#rule.number || R <- Rs])}.

pad_last([{Nt,_Bs,Rs}], Rvsize)     -> [{Nt,Rvsize,Rs}];
pad_last([X = {_,Bs,_}|Xs], Rvsize) -> [X|pad_last(Xs, Rvsize-Bs)].


%%%_. Emacs
%%% Local Variables:
%%% allout-layout: t
%%% End:
