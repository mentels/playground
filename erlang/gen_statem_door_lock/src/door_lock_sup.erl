%%%-------------------------------------------------------------------
%% @doc gen_statem_door_lock top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(door_lock_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link(Opts) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Opts).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init(Opts) ->
    Backend = proplists:get_value(backend, Opts),
    Code = proplists:get_value(code, Opts),
    DoorLock = #{
      id => door_lock,
      start => {Backend, start_link, [Code]}
     },
    {ok, { {one_for_all, 0, 1}, [DoorLock]} }.

%%====================================================================
%% Internal functions
%%====================================================================
