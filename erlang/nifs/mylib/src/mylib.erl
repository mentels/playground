-module(mylib).

%% API exports
-export([benchmark/0]).

%%====================================================================
%% API functions
%%====================================================================

benchmark() ->
    OldCounter = mynif:counter_get(),
    {US, _} = timer:tc(fun do_benchmark/0),
    1000 * 1000 * 10 = mynif:counter_get() - OldCounter, %% sanity check
    US.

%%====================================================================
%% Internal functions
%%====================================================================

do_benchmark() ->
    Pids = [spawn_monitor(fun add_many_times/0) || _ <- lists:seq(1, 1000)],
    [wait_for_proc() || _ <- Pids].

add_many_times() ->
    [mynif:counter_add(10) || _ <- lists:seq(1, 1000)].

wait_for_proc() ->
    receive {'DOWN', _Ref, _Type, _Pid, _Info} -> ok end.
