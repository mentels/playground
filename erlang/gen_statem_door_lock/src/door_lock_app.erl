%%%-------------------------------------------------------------------
%% @doc door_lock public API
%% @end
%%%-------------------------------------------------------------------

-module(door_lock_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(REQUIRED_OPTS, [code, backend]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    door_lock_sup:start_link(opts()).

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

opts() ->
    [{Opt, application:get_env(door_lock, Opt, undefined)}
     || Opt <- ?REQUIRED_OPTS].
