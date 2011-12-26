%%%============================================================================
%%% File     : eburg.hrl
%%% Purpose  : Record, macro, and type definitions.
%%% Author   : j@svirfneblin.org
%%% License  : BSD
%%% Created  : 2007-07-11 14:45:31 j
%%% Modified : $Id: eburg.hrl,v 1.1.1.1 2008/08/07 16:31:33 j Exp $
%%%============================================================================

%%%_. Auto-import
-import(lists,
        [reverse/1, member/2, flatten/1, seq/2, duplicate/2, concat/1, map/2,
         foldl/3, zip/2, foreach/2, all/2, any/2, flatmap/2, usort/1]).


%%%_. Macros
-define(INFINITY, 16777216). % XXX duplicated in burm.hrl

-ifndef(VERBOSE).
-define(VERBOSE(Expr),
        (case get('$verbose') of
             true  -> (Expr);
             false -> ok
         end)).
-endif.

-ifndef(IF).
-define(IF(B,T,F), (case (B) of true -> (T); false -> (F) end)).
-endif.


%%%_. Records and types
-type(name() :: string() | atom()).

%%% An `error_descriptor' is a tuple which `format_error' knows how to
%%% interpret. An `error_info' is an `error_descriptor' bundled with file and
%%% possibly location information.
%%% @todo more detailed types?
-type(error_descriptor() :: tuple() | atom()).
-type(error_info()       :: tuple()).


-record(pattern, {op :: atom(), kids=[]}).

-type(rhs() :: atom() | #pattern{}).

-record(rule, {number      :: pos_integer(),
               line        :: pos_integer(),
               lhs         :: atom(),
               rhs         :: rhs(),
               cost   = 0  :: integer() | string(),
               action = "" :: string()}).

%% Nonterm, #bits to count rules, IRN -> PRN
%-type(proplist(Key, Val) :: [{Key,Val}]).
-type(proplist() :: [{_,_}]).
-type(pmap() :: [{atom(),pos_integer(),proplist()},...]).


-record(eburg, {infile  :: string(),
                outfile :: string(),
                module  :: string(),

                rules   :: [#rule{}],
                code, % XXX
                ops     :: [atom()],
                nts     :: [atom()],
                start   :: atom(),

                rvsize  :: pos_integer(),
                pmap    :: pmap(),

                inns    :: proplist(),
                arities :: proplist(),
                taints  :: proplist(),
                crules  :: proplist(),
                brules  :: proplist()
               }).


%%%_. Emacs
%%% Local Variables:
%%% allout-layout: t
%%% End:
