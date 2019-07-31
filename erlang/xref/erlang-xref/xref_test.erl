-module(xref_test).
-include_lib("eunit/include/eunit.hrl").
-define(SUT, xref_sut).

xref_by_module_test() ->
    ?assertEqual([{deprecated,[]},
                  {undefined,[{{?SUT,exp,1},{?SUT,unexp,1}},
                              {{?SUT,exp,1},{?SUT,z_undef,0}}]},
                  {unused,[{?SUT,unexp,1}]}],
                 xref:m(?SUT)).

xref_by_directory_test() ->
    ?assertEqual([{deprecated,[]},
                  {undefined,[{{?SUT,exp,1},{?SUT,unexp,1}},
                              {{?SUT,exp,1},{?SUT,z_undef,0}}]},
                  {unused,[{?SUT,unexp,1}]}],
                 xref:d(".")).

xref_server_test() ->
    catch xref:stop(s),
    {ok, _Pid} = xref:start(s),
    {ok, ?SUT} = xref:add_module(s, ?SUT),
    Analyses = [undefined_function_calls, locals_not_used],
    ?assertMatch([
                  _Undefs = [{{?SUT,exp,1},{?SUT,unexp,1}},
                             {{?SUT,exp,1},{?SUT,z_undef,0}}],
                  _UnusedLocals = [{?SUT,unexp,1}]
                 ],
                 [element(2, xref:analyse(s, A)) || A <- Analyses]).

%% All the functions that are externally used (XU) and unknown (U)
xref_xu_u_query_test() ->
    catch xref:stop(s),
    {ok, _Pid} = xref:start(s),
    {ok, ?SUT} = xref:add_module(s, ?SUT),
    ?assertMatch({ok, [{?SUT,z_undef,0}]}, xref:q(s, "XU * U")).

%% All the modules that end with .*_sut
xref_mod_regexp_query_test() ->
    catch xref:stop(s),
    {ok, _Pid} = xref:start(s),
    {ok, ?SUT} = xref:add_module(s, ?SUT),
    ?assertMatch({ok, [?SUT]}, xref:q(s, '".*_sut" : Mod')).

%% All the exported functions from modules that end with .*_sut
xref_mod_regexp_and_exports_query_test() ->
    catch xref:stop(s),
    {ok, _Pid} = xref:start(s),
    {ok, ?SUT} = xref:add_module(s, ?SUT),
    ?assertMatch({ok, [{?SUT,exp,1}]},
                 xref:q(s, '".*_sut" : Mod * X')).

xref_query_matching_predefined_analysis_test_SKIP() ->
    {timeout, 60,
     fun() ->
             catch xref:stop(s),
             {ok, _Pid} = xref:start(s),
             ok = xref:set_default(s, [{verbose,false},{warnings,false}]),
             {ok, _} = xref:add_release(s, code:lib_dir(), {name, otp}),
             ?assert(xref:analyse(s, exports_not_used) =:= xref:q(s, "X - XU"))
     end}.

xref_runner_locals_not_used_test() ->
    ?assertMatch([#{source := {?SUT,unexp,1}}],
                 xref_runner:check(locals_not_used, #{dir => "."})).

xref_runner_reads_config_from_file_test() ->
    true = filelib:is_file("xref.config"),
    ?assertMatch([#{source := {?SUT,unexp,1}}], xref_runner:check()).
