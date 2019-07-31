-module(l2s).
-compile([export_all]).

%% API

start_sup() ->
    supervisor:start_link(?MODULE, []).

start_proc_and_link(Pid) ->
    spawn(fun() ->
                  link(Pid),
                  receive do -> exit(ala) end
          end).

%% Callbacks

init(_) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 5,
                 period => 60},
    ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}}.
