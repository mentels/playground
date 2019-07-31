-module(gen_statem_door_lock).
-behaviour(gen_statem).

-export([start_link/1]).
-export([button/1]).
-export([code_length/0]).
-export([init/1,callback_mode/0,terminate/3]).
-export([locked/3,open/3]).

-define(CLOSE_S, 5).
-define(HANDLE_COMMON,
        ?FUNCTION_NAME(Type, Content, Data) -> handle_common(Type, Content, Data)).

%%% API

start_link(Code) when is_list(Code) ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, Code, []).

button(Button) when is_integer(Button) ->
    gen_statem:cast(?MODULE, {button, Button}).

code_length() ->
    gen_statem:call(?MODULE, code_length).

%% Behaviour Callbacks

init(Code) ->
    {ok, locked, #{code => Code, length => length(Code), buttons => []}}.

callback_mode() -> state_functions.

terminate(_Reason, State, _Data) ->
    State =/= locked andalso do_lock(),
    ok.

%% State Callbacks


locked(cast, {button, Button},
       #{code := Code, length := Length, buttons := Buttons} = Data) ->
    NewButtons = case length(Buttons) of
        Length -> tl(Buttons);
        _ -> Buttons
    end ++ [Button],
    case NewButtons of
        Code ->
            do_unlock(),
            %% when the state is changed the timer is cancelled
            %% it is also re-set when new state_timeout action is set
            {next_state, open, Data#{buttons := []},
             [{state_timeout, ?CLOSE_S*1000, lock}]};
        _Other ->
            {next_state, locked, Data#{buttons := NewButtons}}
    end;
?HANDLE_COMMON.

open(state_timeout, lock, Data) ->
    do_lock(),
    {next_state, locked, Data};
open(cast, {button, _}, _Data) ->
    keep_state_and_data; %% {keep_state, Data} would work too.
?HANDLE_COMMON.

%% Internal Functions

handle_common({call, From}, code_length, #{length := Length} = Data) ->
    {keep_state, Data, [{reply, From, Length}]}.

do_unlock() ->
    io:format("The door has been unlocked... Will get locked in ~p s", [?CLOSE_S]).

do_lock() ->
    io:format("The door has been locked... ").
