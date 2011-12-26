%%%============================================================================
%%% File     : eburg_parser.yrl
%%% Purpose  : Grammar for eburg specifications.
%%% Author   : j@svirfneblin.org
%%% License  : BSD
%%% Created  : 2007-06-07 10:53:58 j
%%% Modified : $Id: eburg_parser.yrl,v 1.1.1.1 2008/08/07 16:31:33 j Exp $
%%%============================================================================

Nonterminals spec term start ops rules rule trees tree.
Terminals 'Start' 'Terminals' nt op action cost '<-' '(' ')' ',' '.'.
Rootsymbol spec.
Endsymbol '$end'.


%% BURG specification
spec  -> term start rules : #eburg{ops='$1', start='$2', rules='$3'}.
spec  -> start term rules : #eburg{ops='$2', start='$1', rules='$3'}.
spec  -> term rules       : #eburg{ops='$1', start=first_nt('$2'), rules='$2'}.

%% Declarations
term  -> 'Terminals' ops '.' : '$2'.
start -> 'Start' nt '.'      : unwrap('$2').
ops   ->  op                 : [unwrap('$1')].
ops   ->  op ops             : [unwrap('$1')|'$2'].

%% Rules
rules -> rule                     : ['$1'].
rules -> rule rules               : ['$1'|'$2'].

rule  -> nt '<-' tree '.'         : #rule{line   = line('$1'),
                                          lhs    = unwrap('$1'),
                                          rhs    = '$3'}.

rule  -> nt '<-' tree cost '.'    : #rule{line   = line('$1'),
                                          lhs    = unwrap('$1'),
                                          rhs    = '$3',
                                          cost   = unwrap('$4')}.

rule  -> nt '<-' tree action      : #rule{line   = line('$1'),
                                          lhs    = unwrap('$1'),
                                          rhs    = '$3',
                                          action = unwrap('$4')}.

rule  -> nt '<-' tree cost action : #rule{line   = line('$1'),
                                          lhs    = unwrap('$1'),
                                          rhs    = '$3',
                                          cost   = unwrap('$4'),
                                          action = unwrap('$5')}.

%% RHSs
trees -> tree              : ['$1'].
trees -> tree ',' trees    : ['$1'|'$3'].
tree  -> nt                : unwrap('$1').
tree  -> op                : #pattern{op=unwrap('$1')}.
tree  -> op '(' trees  ')' : #pattern{op=unwrap('$1'), kids='$3'}.


Erlang code.
-include("eburg.hrl").

-spec(unwrap/1 :: ({_,integer(),_}) -> _).
%% @doc Extract content from a Leex token.
unwrap({_,_,Content}) -> Content.


-spec(line/1 :: ({_,integer(),_}) -> integer()).
%% @doc Extract line number from Leex token.
line({_,Line,_}) -> Line.


-spec(first_nt/1 :: ([#rule{}]) -> atom()).
%% @doc Returns the implicit start nonterminal.
first_nt(Rs) -> (hd(Rs))#rule.lhs.


%%% eof
