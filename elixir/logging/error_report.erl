-module(error_report).
-compile([export_all]).
-behavoiur(gen_server).
-behavoiur(supervisor).

start() ->
    {ok, _Pid} = supervisor:start_link(?MODULE, sup),
    exit(erlang:whereis(srv), kill_him).

start_srv() ->
    gen_server:start_link(?MODULE, srv, []).

init(sup) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    ChildSpecs = [#{id => srv,
                    start => {?MODULE, start_srv, []},
                    restart => temporary,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [?MODULE]}],
    {ok, {SupFlags, ChildSpecs}};
init(srv) ->
    register(srv, self()),
    {ok, []}.
