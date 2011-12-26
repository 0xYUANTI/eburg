%%%============================================================================
%%% File     : eburg_util.erl
%%% Purpose  : Utility procedures.
%%% Author   : j@svirfneblin.org
%%% License  : BSD
%%% Created  : 2007-09-20 13:00:39 j
%%% Modified : $Id: eburg_util.erl,v 1.1.1.1 2008/08/07 16:31:33 j Exp $
%%%============================================================================

%%%_. Attributes

-module(eburg_util).
-author('j@svirfneblin.org').

-export([sget/3, assoc/2, is_tainted/2, has_closure/2, irn2prn/3,
         die/1, maybe_die/2,
         c/1, c/2, e/1, e/2, mkcode/3, emitcode/3, emitcode2/3,
         gsub/3, match/2,
         dumprules/1, unparse_rule_short/1,
         uniq/1, enumerate/1, gensym/1, pp/1, mk_reader/2, next_pot/1,next_moe/1
        ]).

-include("eburg.hrl").


%%%_. Operator & nonterm properties

-spec(sget/3 :: (OpOrNt :: atom(),Prop :: atom(),St :: #eburg{}) -> _).
%% @doc Returns the value associated with `OpOrNt's property `Prop' in `St'.
sget(Op, arity , St) -> assoc(Op, St#eburg.arities);
sget(Op, taints, St) -> assoc(Op, St#eburg.taints);
sget(Op, brules, St) -> assoc(Op, St#eburg.brules);
sget(Nt, inn   , St) -> assoc(Nt, St#eburg.inns);
sget(Nt, crules, St) -> assoc(Nt, St#eburg.crules).


-spec(assoc/2 :: (T1,[{T1,T2}]) -> T2 | none).
%% @doc Returns the value associated with `Key' in `Alist'.
assoc(Key, Alist) ->
    case proplists:lookup(Key, Alist) of
        {Key, Val} -> Val;
        none       -> none
    end.


-spec(is_tainted/2 :: (atom(),#eburg{}) -> bool()).
%% @doc Operators are tainted if any of their baserules or any of the
%% chainrules which occur in the closures of their baserules are dynamic.
is_tainted(Op, St) -> sget(Op, taints, St) =/= [].


-spec(has_closure/2 :: (atom(),#eburg{}) -> bool()).
%% @doc Returns true if `Nt' has a chainrule closure.
has_closure(Nt, St) -> sget(Nt, crules, St) =/= [].


-spec(irn2prn/3 :: (atom(),integer(),#eburg{}) -> integer()).
%% @doc Returns the packed rule number corresponding to the internal rule
%% number `IRN'.
irn2prn(Nt, IRN, St) ->
    {value,{Nt,_,I2P}} = lists:keysearch(Nt, 1, St#eburg.pmap),
    assoc(IRN, I2P).


%%%_. Error handling

-spec(die/1 :: (error_info()) -> no_return()).
%% @doc Prints an error message and exits (by throwing `error', which is only
%% caught in `eburg:file' and hence fatal).
die(Descr) -> err(1, Descr), throw(error).


-spec(maybe_die/2 :: ([error_info()],[error_info()]) -> true | no_return()).
%% @doc Iterates over `Errs' and `Warns' and prints appropriate messages.
%% Exits if `Errs' is non-nil.
maybe_die(Errs, Warns) ->
    foreach(fun (W) -> err(0, W) end, Warns),
    foreach(fun (E) -> err(1, E) end, Errs),
    Errs == [] orelse throw(error).


-spec(err/2 :: (0..1,error_info()) -> ok).
%% @doc Prints an Emacs-friendly error message.
%% `Level' indicates the message type (warning or error).
err(Level, Enfo) -> message(prefix(Level), Enfo).

prefix(0) -> "Warning: ";
prefix(1) -> "".

message(Prefix, {File,Line,Reason}) ->
    Msg = Prefix ++ format_error(Reason),
    io:format("~s:~w: ~s~n", [File, Line, Msg]);
message(Prefix, {File,Reason}) ->
    Msg = Prefix ++ format_error(Reason),
    io:format("~s: ~s~n", [File, Msg]).


-spec(format_error/1 :: (Reason :: error_descriptor()) -> string()).
%% @doc Returns a descriptive error message for `Reason'.
format_error({file,Reason}) ->
    file:format_error(Reason);
format_error({syntax,Module,Descr}) ->
    "bad syntax: " ++ apply(Module, format_error, [Descr]);
format_error({code,Module,Descr}) ->
    "bad user code: " ++ apply(Module, format_error, [Descr]);
format_error({action,Module,Descr}) ->
    "bad semantic action: " ++ apply(Module, format_error, [Descr]);
format_error({cost,Module,Descr}) ->
    "bad dynamic cost: " ++ apply(Module, format_error, [Descr]);
format_error({dup,Ops}) ->
    io_lib:format("multiple operator declarations: ~p", [Ops]);
format_error(start) ->
    "no rules for declared start symbol";
format_error({use,Op}) ->
    io_lib:format("declared as operator but used as nonterminal: ~p", [Op]);
format_error({undeclared,Op}) ->
    io_lib:format("used as operator but not declared: ~p", [Op]);
format_error({arity,Op,Ns}) ->
    io_lib:format("operator used with different arities: ~p: ~p", [Op, Ns]);
format_error({unused,Op}) ->
    io_lib:format("operator not used: ~p", [Op]);
format_error({unreachable,Nt}) ->
    io_lib:format("nonterminal not reachable from start symbol: ~p", [Nt]).


%%%_. Code generation

%%% We use two basic operators for code generation.
%%% `c' instatiates `Template' with `Args' and returns the resulting code.
-spec(c/1 :: (string()) -> string()).
c(Template) -> flatten(io_lib:format("~s", [Template])).
-spec(c/2 :: (string(),any()) -> string()).
c(Template, Args) -> flatten(io_lib:format(Template, Args)).
%%% `e' emits `Template' followed by a newline to the stream stored as
%%% '$out_stream' in the process dictionary (possibly filling in `Args' first).
-spec(e/1 :: (string()) -> ok).
e(Template) -> io:format(get('$out_stream'), "~s~n", [Template]).
-spec(e/2 :: (string(),any()) -> ok).
e(Template, Args) -> io:format(get('$out_stream'), Template ++ "~n", Args).


%%               Open Delim Close
table(func)  -> {""  , ";",  "."};
table(pfunc) -> {""  , ";",  ";"}; % partial function
table(cbody) -> {""  , ";",   ""}; % case body
table(bin)   -> {"<<", ",", ">>"};
table(tuple) -> {"{" , ",",  "}"};
table(list)  -> {"[" , ",",  "]"};
table(sum)   -> {""  , "+",   ""}; % XXX parens?
table(blank) -> {""  ,  "",   ""}.


-spec(mkcode/3 :: (atom(),string(),[_]) -> string()).
%% @doc Returns code for a `Type' (usually bin, tuple, &c.);
%% elements are generated by instatiating `Template' for all `Args'.
mkcode(Type, Template, Args) ->
    {O,D,C} = table(Type),
    O ++ mkcode_(Template, Args, D) ++ C.

mkcode_(_, [], _) ->
    "";
mkcode_(Template, [Args], _ ) ->
    c(Template, unpack(Args));
mkcode_(Template, [Args|MoreArgs], Delim) ->
    c(Template, unpack(Args))
        ++ c(Delim)
        ++ mkcode_(Template, MoreArgs, Delim).


-spec(emitcode/3 :: (atom(),string(),[_]) -> ok).
%% @doc Emits code for a `Type'; the constituents are constructed by
%%% instantiating `Template' once for each element of `Args'.
emitcode(_, _, []) -> ok;
emitcode(Type, Template, Args) ->
    {O,D,C} = table(Type),
    e(O), emitcode_(Template, Args, D), e(C).

emitcode_(_, [], _) ->
    ok;
emitcode_(Template, [Args], _) ->
    e(Template, unpack(Args));
emitcode_(Template, [Args|MoreArgs], Delim) ->
    e(Template, unpack(Args)),
    e(Delim),
    emitcode_(Template, MoreArgs, Delim).


-spec(unpack/1 :: ({T}|T) -> [T]).
%% @doc Converts `Args' into an argument list for `c' and `e'.
unpack(Args) when is_tuple(Args) -> tuple_to_list(Args);
unpack(Arg) -> [Arg].


-spec(emitcode2/3 :: (atom(),fun(),[_]) -> ok).
%% @doc Emits code for a `Type'; this is for those few cases where the body of
%% the statement we want to generate cannot be expressed as a simple format
%% template, so we use a custom printer-function instead.
emitcode2(_, _, []) -> ok;
emitcode2(Type, Printer, Args) ->
    {O,D,C} = table(Type),
    e(O), emitcode2_(Printer, Args, D), e(C).

emitcode2_(_, [], _) ->
    ok;
emitcode2_(Printer, [Args], _) ->
    Printer(Args);
emitcode2_(Printer, [Args|MoreArgs], Delim) ->
    Printer(Args),
    e(Delim),
    emitcode2_(Printer, MoreArgs, Delim).


%%%_. Regexps
%%% We only match and replace plain strings.

-spec(gsub/3 :: (string(),string(),string()) -> string()).
%% @doc Substitute `RE' by `Subst' in `Str'.
gsub(Str, RE, Subst) -> regexp:gsub(Str, escape(RE), Subst).


-spec(match/2 :: (string(),string()) -> bool()).
%% @doc Returns true if `RE' occurs in `Str'.
match(Str, RE) ->
    case regexp:match(Str, escape(RE)) of
        nomatch -> false;
        _       -> true
    end.


-spec(escape/1 :: (string()) -> string()).
%% @doc Returns Cs with any regexp meta characters escaped.
escape([C|Cs]) -> ?IF(escape_p(C), [$\\, C|escape(Cs)], [C|escape(Cs)]);
escape([])     -> [].


-spec(escape_p/1 :: (char()) -> bool()).
%% @doc Returns `true' if `C' is a regexp meta character.
escape_p(C) -> member(C, [$\\, $., $^, $$, $[, $], $|, $+, $*, $?, $(, $)]).


%%%_. Debugging helpers

-spec(dumprules/1 :: ([#rule{}]) -> ok).
%% @doc Prints `Rs' to stdout.
dumprules(Rs) ->
    foreach(fun(R) -> io:put_chars(unparse_rule(R)) end, Rs).


-spec(unparse_rule/1 :: (#rule{}) -> string()).
%% @doc Returns the full string representation of `R'.
unparse_rule(#rule{number=N, lhs=LHS, rhs=RHS, cost=C, action=A}) ->
    io_lib:format("~p. ~p <- ~p [~p] :", [N, LHS, unparse_rhs(RHS), C])
        ++ A ++ "\n".


-spec(unparse_rule_short/1 :: (#rule{}) -> string()).
%% @doc Returns the short string representation of `R'.
unparse_rule_short(#rule{lhs=LHS, rhs=RHS}) ->
    io_lib:format("~p <- ~s", [LHS, unparse_rhs(RHS)]).


-spec(unparse_rhs/1 :: ([rhs()]|rhs()) -> string()).
%% @doc Returns the string representation of `RHS'.
unparse_rhs([]) ->
    "";
unparse_rhs([X]) ->
    unparse_rhs(X);
unparse_rhs([X|Xs]) ->
    unparse_rhs(X) ++ ", " ++ unparse_rhs(Xs);
unparse_rhs(Nt) when is_atom(Nt) ->
    atom_to_list(Nt);
unparse_rhs(#pattern{op=Op, kids=[]}) ->
    atom_to_list(Op);
unparse_rhs(#pattern{op=Op, kids=Kids}) ->
    atom_to_list(Op) ++ "(" ++ unparse_rhs(Kids) ++ ")".


%%%_. Misc

-spec(uniq/1 :: ([T]) -> [T]).
%% @doc Returns `Xs' with duplicate elements removed.
uniq([X|Xs]) -> [X|uniq([Y || Y <- Xs, Y =/= X])];
uniq([])     -> [].


-spec(enumerate/1 :: ([T]) -> [{T,pos_integer()}]).
%% @doc Returns `Xs' with sequence numbers attached to its elements.
enumerate(Xs) -> zip(Xs, seq(1, length(Xs))).


-spec(gensym/1 :: (atom()) -> atom()
                ; (string()) -> string()).
%% @doc Implements a simple gensym-like facility for strings and atoms.
gensym(Base) when is_atom(Base) ->
    Str0 = atom_to_list(Base),
    Str1 = gensym(Str0),
    list_to_atom(Str1);
gensym(Base) when is_list(Base) ->
    Sym = [$_,$_|Base],
    N = case get(Sym) of
            undefined -> 0;
            X -> X + 1
        end,
    put(Sym, N),
    Base ++ integer_to_list(N).


-spec(pp/1 :: (string()) -> ok).
%% @doc Pretty-prints `File'.
pp(File) -> erl_tidy:file(File, [{backups,false}, idem]).


-spec(mk_reader/2 :: (fun(),fun()) -> fun()).
%% @doc Returns a function which uses `Lexer' and `Parser' (assumed to follow
%% yecc/stdlib conventions) to tokenize and parse its single argument
%% (which must be a string).
mk_reader(Lexer, Parser) ->
    fun (String) ->
            case Lexer(String) of
                {error, Reason, _Line} -> {error, Reason};
                {ok, Tokens, _Line} ->
                    case Parser(Tokens) of
                        Err={error, _Reason} -> Err;
                        {ok, Result} -> Result
                    end
            end
    end.


-spec(next_pot/1 :: (pos_integer()) -> pos_integer()).
%% @doc Returns the next power of two >= `N'.
next_pot(N) -> trunc(math:log(N) / math:log(2)) + 1.


-spec(next_moe/1 :: (pos_integer()) -> pos_integer()).
%% @doc Returns the next multiple of eight >= `N'.
next_moe(N) ->
    case N rem 8 of
        0 -> N;
        R -> N + 8 - R
    end.


%%%_. Emacs
%%% Local Variables:
%%% allout-layout: t
%%% End:
