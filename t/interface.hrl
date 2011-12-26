%%%============================================================================
%%% File     : interface.hrl
%%% Purpose  : BURM interface.
%%% Author   : j@svirfneblin.org
%%% License  : BSD
%%% Created  : 2007-09-22 16:15:52 j
%%% Modified : $Id: interface.hrl,v 1.1.1.1 2008/08/07 16:31:33 j Exp $
%%%============================================================================

%% The tree ADT

%% This, you typically have already.
-record(tree, {op                :: atom(),
               kids        = [],%:: [#tree{}]
               val         = 0   :: _,
               annotations = []  :: [_],
               state}).         %:: #burm_state{}

%% These, you need to define.
kids(T)                   -> T#tree.kids.
kids_set(T, Kids)         -> T#tree{kids=Kids}.
state_label(T)            -> T#tree.state.
state_label_set(T, State) -> T#tree{state=State}.

op_label(T) ->
    case T#tree.op of
        'CNSTI' -> % iburg/sample4
            if T#tree.val =:= 0 -> 'I0I';
               true             -> T#tree.op
            end;
        Op -> Op
    end.

%%-----------------------------------------------------------------------------
%% Everything below this line is optional

%% Access subject tree
val(Node)    -> Node#tree.val.
val(N, Node) -> (lists:nth(N, kids(Node)))#tree.val.
kid(N, Node) -> lists:nth(N, kids(Node)).

%% annotate :: tree -> tree
annotate(Node, A) ->
    Node#tree{annotations=[A|Node#tree.annotations]}.
annotate(Node, A, Kids) ->
    (annotate(Node, A))#tree{kids=Kids}.

%% constructors
l(Op)            -> #tree{op=Op}.
l(Op, Val)       -> #tree{op=Op, val=Val}.
n(Op, Kids)      -> #tree{op=Op, kids=Kids}.
n(Op, Val, Kids) -> #tree{op=Op, val=Val, kids=Kids}.

%% Clear tables
ets_reset() ->
    ets:delete(burm_id_cache),
    ets:delete(burm_state_cache),
    ets:delete(burm_swdc_cache).

%%% Local Variables:
%%% mode: erlang
%%% End:
