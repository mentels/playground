-module(logging_lager).

-compile([export_all, {parse_transform, lager_transform}]).

-define(WATERMARK, 100).
-define(PROCS, 100).

setup() ->
    ok = application:set_env(lager, error_logger_hwm, ?WATERMARK),
    %% ok = application:set_env(lager, lager_file_backend, [{file, "console.log"}, {level, error}]).
    ok = lager:set_loglevel(lager_file_backend, "console.log", info).
    %% ok = lager:set_loglevel(lager_file_backend, "console.log", info).

check_setup() ->
    application:load(lager),
    application:get_all_env(lager).


exceed_error_logger_hwm() ->
    setup(),
    MsgsPerProc = ?WATERMARK*2,
    Parent = self(),
    Pids = lists:map(fun(ProcId) ->
                             Start = MsgsPerProc*ProcId,
                             End = Start + (MsgsPerProc-1),
                             Fun = fun error_logger:info_msg/2,
                             %% Fun = fun(Fmt, Args) -> lager:info(Fmt, Args) end,
                             spawn(fun() -> log_exceedingly(Parent, Fun, {Start, End}) end)
                     end, lists:seq(0, ?PROCS-1)),
    lists:foreach(fun(Pid) -> Pid ! {go, Parent} end, Pids).


log_exceedingly(Parent, Fun, {Start, End}) ->
    receive
        {go, Parent} ->
            [Fun("[~p] [~p] Log message", [self(), I]) || I <- lists:seq(Start, End)]
    after 1000 ->
            lager:error("[~p] Failed to recive start signal", [self()]),
            exit(timeout)
    end.
