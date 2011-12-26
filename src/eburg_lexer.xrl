%%%============================================================================
%%% File     : eburg_lexer.xrl
%%% Purpose  : Scanner & tokenizer for eburg specifications.
%%% Author   : j@svirfneblin.org
%%% License  : BSD
%%% Created  : 2007-07-07 10:40:08 j
%%% Modified : $Id: eburg_lexer.xrl,v 1.1.1.1 2008/08/07 16:31:33 j Exp $
%%%============================================================================

Definitions.
LOWER     = [a-z]
UPPER     = [A-Z]
DIGIT     = [0-9]
CHAR      = ({LOWER}|{UPPER})
WS        = [\000-\s]+

NONTERM   = {LOWER}({LOWER}|{DIGIT}|_)*
OPERATOR  = {UPPER}({UPPER}|{DIGIT}|_)*
DOT       = \.[\n\s\t]
COMMENT   = \%.*
EOF       = \$end

DCOST     = \[:([^]]|[^:]\])*:\]
ACTION    = :([^.]|\.[^\n\s\t])*{DOT}


Rules.
Terminals  : {token, {'Terminals', TokenLine}}.
Start      : {token, {'Start', TokenLine}}.

{NONTERM}  : {token, {nt, TokenLine, list_to_atom(TokenChars)}}.
{OPERATOR} : {token, {op, TokenLine, list_to_atom(TokenChars)}}.

{ACTION}   : {token, {action, TokenLine, stripa(TokenChars)}}.

{DCOST}    : {token, {cost, TokenLine, stripc(TokenChars)}}.
{DIGIT}+   : {token, {cost, TokenLine, list_to_integer(TokenChars)}}.

<-         : {token, {'<-', TokenLine}}.
\(         : {token, {'(',  TokenLine}}.
\)         : {token, {')',  TokenLine}}.
,          : {token, {',',  TokenLine}}.
{DOT}      : {token, {'.',  TokenLine}}.

{WS}       : skip_token.
{COMMENT}  : skip_token.
{EOF}      : {end_token, {'$end', TokenLine}}.


Erlang code.
-export([lex/1]).

-spec(lex/1 :: (string()) -> string()).
%% @doc Lexes `File'. See EOF above.
lex(File) -> string(File ++ "$end").


stripc(Cost)   -> tl(tl(butlast(butlast(Cost)))).
stripa(Action) -> tl(butlast(butlast(Action))).


-spec(butlast/1 :: ([T,...]) -> [T]).
%% @doc Returns `Xs' without its last element.
butlast([_X])   -> [];
butlast([X|Xs]) -> [X|butlast(Xs)].


%%% eof
