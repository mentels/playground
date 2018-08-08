%% Compile:
%% erlc *
%% Run tests:
%% module_lifecycle:test().
%% or
%% eunit:test(module_lifecycle, [verbose]).
-module(module_lifecycle).

%% test() function will be auto-exported
-include_lib("eunit/include/eunit.hrl").

-define(MOD, "dummy_mod").
-define(MOD_PATH, ?MOD ++ ".erl").

%% API

%% Helpers

compile(File) ->
    {ok, ModuleName, ModuleBin} = compile:file(File, [binary]),
    {ModuleName, ModuleBin}.

load(ModuleName, ModuleBinary) ->
    {module, ModuleName} =code:load_binary(ModuleName, 'no_file', ModuleBinary),
    ok.

%% Tests

purge_then_delete_does_not_remove_the_code_test() ->
    %% GIVEN
    process_flag(trap_exit, true),
    {ModuleName, ModuleBin} = compile(?MOD_PATH),
    ?assertNot(code:is_loaded(ModuleName)),
    ok = load(ModuleName, ModuleBin),

    %% WHEN
    Pid = spawn_link(fun() -> ModuleName:f() end),
    erlang:monitor(process, Pid),

    %% THEN
    %% the module is not purged as it has no version marked as old
    %% http://erlang.org/doc/search/?q=&x=25&y=3
    ?assertNot(code:purge(ModuleName)),
    %% the current version of the module is marked old
    %% http://erlang.org/doc/search/?q=&x=25&y=3
    ?assert(code:delete(ModuleName)),
    %% the process still runs the old version of the module
    ?assert(erlang:is_process_alive(Pid)),

    %% CLEANUP
    code:purge(ModuleName).

delete_then_purge_does_remove_the_code_test() ->
    %% GIVEN
    process_flag(trap_exit, true),
    {ModuleName, ModuleBin} = compile(?MOD_PATH),
    ?assertNot(code:is_loaded(ModuleName)),
    ok = load(ModuleName, ModuleBin),

    %% WHEN
    Pid = spawn_link(fun() -> ModuleName:f() end),
    Ref = erlang:monitor(process, Pid),

    %% THEN
    %% the current version of the module is marked old
    %% http://erlang.org/doc/search/?q=&x=25&y=3
    ?assert(code:delete(ModuleName)),
    %% the module is purged
    ?assert(code:purge(ModuleName)),
    %% the process running the purged module was killed
    receive {'DOWN', Ref, process, Pid, killed} -> ok
    after 500 -> throw(no_down_message_received) end.
