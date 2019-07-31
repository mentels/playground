-module(line_port_SUITE).
-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

-define(C_PORT, "string_reverser.o").

all() -> [line_below_80, line_below_80_wo_newline, line_above_80].

init_per_suite(Config) ->
    DataDir = ?config(data_dir, Config),
    [{runnable, filename:join([DataDir, ?C_PORT])}].

end_per_suite(_Config) ->
    ok.

line_below_80(Config) ->
    String = "alutka",
    Port = open_port({spawn, ?config(runnable, Config)},
                     [stream, {line, 80}]),
    port_command(Port, String ++ [$\n]),
    collect_result(Port, String, _StartPoint = 1).

line_below_80_wo_newline(Config) ->
    String = "alutka",
    Port = open_port({spawn, ?config(runnable, Config)},
                     [stream, {line, 80}]),
    port_command(Port, String),
    ?assertExit(string_not_reversed,
                collect_result(Port, String, _StartPoint = 1)).


%% @doc Many messages will be sent from the port as it stores at most 79
%% characters at once. Before it moves on with reading the rest of the stdin
%% it returns the reply to the current chunk.
line_above_80(Config) ->
    String = lists:flatten([
                             [ "_" |integer_to_list(N)]
                             || N <- lists:seq(1, 100)]),
    Port = open_port({spawn, ?config(runnable, Config)},
                     [stream, {line, 80}]),
    port_command(Port, String ++ [$\n]),
    collect_result(Port, String, _StartPoint = 1).

collect_result(Port, String, StartPoint)
  when StartPoint < length(String)->
    receive
        {Port, {data, {eol, Result}}} ->
            ?assertEqual(lists:reverse(
                           %% THIS FAILS
                           %% string:sub_string(String, 1, 80)),
                           %%
                           %% THIS WORKS
                           string:sub_string(String,
                                             StartPoint,
                                             %% The stringi s at most 79 characters long
                                             StartPoint + 79 - 1)),
                         Result),
            ct:pal("Collected ~p~n",[Result]),
            collect_result(Port, String, StartPoint+79)
    after
        1000 ->
            exit(string_not_reversed)
    end;
collect_result(_, _, _) ->
    ok.
