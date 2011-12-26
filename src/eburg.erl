%%%============================================================================
%%% File     : eburg.erl
%%% Purpose  : A code generator generator for Erlang.
%%% Author   : j@svirfneblin.org
%%% License  : BSD
%%% Created  : 2007-07-22 15:49:04 j
%%% Modified : $Id: eburg.erl,v 1.1.1.1 2008/08/07 16:31:33 j Exp $
%%%============================================================================

-module(eburg).
-author('j@svirfneblin.org').

-import(eburg_frontend, [read/2]).
-import(eburg_backend,  [emit/1]).

-export([file/1, file/2, main/2]).

-include("eburg.hrl").


-spec(file/1 :: (name()) -> ok | error).
%% @equiv file(File, [])
file(File) ->
    file(File, []).


-spec(file/2 :: (name(),proplist()) -> ok | error).
%% @doc Returns `error' if there are errors or `ok' if there aren't.
%% `File' is the input specification. If there are no errors, the code
%% generator is written to "Base.erl" where `Base' is the basename of
%% `File'.
%%
%% Options:
%% <dl>
%%   <dt>`{outdir, string()}'</dt>
%%   <dt>`{verbose, bool()}'</dt>
%% </dl>
%% @end
file(File, Opts) ->
    %cprof:stop(),
    %cprof:start(),
    Me = self(),
    spawn_opt(fun() -> Me ! (catch emit(read(File, Opts))) end,
              [{min_heap_size, 10000}]),
    receive
        Result ->
            %cprof:pause(),
            Result
    end.


-spec(main/2 :: (name(),proplist()) -> no_return()).
%% @doc For command line use. Halts the Erlang runtime after calling `file';
%% reports success or failure to the environment via its exit status.
main(File, Opts) ->
    case file(File, Opts) of
        ok    -> halt(0);
        error -> halt(1)
    end.


%%% eof
