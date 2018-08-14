-module(door_lock_state_based).
-behaviour(gen_statem).

-export([start_link/1]).
-export([button/1]).
-export([code_length/0]).
-export([init/1,callback_mode/0,terminate/3]).
-export([locked/3,open/3]).

-define(CLOSE_S, 5).
-define(HANDLE_COMMON,
        ?FUNCTION_NAME(Type, Content, Data) -> handle_common(Type, Content, Data)).
-define(BUTTON_TIMEOUT_S, 5).

%%% API

start_link(Code) when is_list(Code) ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, Code, []).

down(Button) when is_integer(Button) ->
    gen_statem:cast(?MODULE, {down, Button}).

up(Button) when is_integer(Button) ->
    gen_statem:cast(?MODULE, {up, Button}).

code_length() ->
    gen_statem:call(?MODULE, code_length).

%%% Tests Helpers

button(Button) when is_integer(Button) ->
    down(Button),
    up(Button).

%% Behaviour Callbacks

init(Code) ->
    {ok, locked, #{code => Code, length => length(Code), buttons => []}}.

callback_mode() -> [state_functions, state_enter].

terminate(_Reason, State, _Data) ->
    State =/= locked andalso do_lock(terminate),
    ok.

%% State Callbacks


locked(enter, OldState, Data) ->
    do_lock(OldState),
    {keep_state, Data#{buttons => []}};
locked(timeout, _, Data) ->
    io:format("Button sequence reset buttons=~p~n", [maps:get(buttons, Data)]),
    {keep_state, Data#{buttons => []}};
locked(internal, {button, Button},
       #{code := Code, length := Length, buttons := Buttons} = Data) ->
    NewButtons = case length(Buttons) of
        Length -> tl(Buttons);
        _ -> Buttons
    end ++ [Button],
    case NewButtons of
        Code ->
            {next_state, open, Data};
        _Other ->
            %% {timeout,Time,Content} can also be used to make
            %% the event timeout carry more data
            {next_state, locked, Data#{buttons := NewButtons}, ?BUTTON_TIMEOUT_S*1000}
    end;
?HANDLE_COMMON.

open(enter, OldState, _Data) ->
    do_unlock(OldState),
    %% when the state is changed the timer is cancelled
    %% it is also re-set when a new state_timeout action is set
    {keep_state_and_data, [{state_timeout, ?CLOSE_S*1000, lock}]};
open(state_timeout, lock, Data) ->
    {next_state, locked, Data};
open(internal, {button, _}, _Data) ->
    {keep_state_and_data, [postpone]}; %% {keep_state, Data} would work too.
?HANDLE_COMMON.

%% Internal Functions

handle_common({call, From}, code_length, #{length := Length} = Data) ->
    {keep_state, Data, [{reply, From, Length}]};
handle_common(cast, {down, Button}, Data) ->
    {keep_state, Data#{button => Button}};
handle_common(cast, {up, Button}, Data) ->
    case Data of
        #{button := Button} ->
            {keep_state, maps:remove(button, Data),
             [{next_event, internal, {button, Button}}]};
        _ ->
            keep_state_and_data
    end.


do_unlock(OldState) ->
    io:format("The door has been unlocked after state=~p..."
              "Will get locked in ~p s~n", [OldState, ?CLOSE_S]).

do_lock(OldState) ->
    io:format("The door has been locked after state=~p... ~n", [OldState]).
