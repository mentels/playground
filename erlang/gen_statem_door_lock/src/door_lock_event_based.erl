-module(door_lock_event_based).
-behvaour(gen_statem).

-export([start_link/1]).
-export([button/1]).
-export([code_length/0]).
-export([init/1,callback_mode/0,terminate/3]).
-export([handle_event/4]).

-define(BUTTON_TIMEOUT_S, 5).
-define(CLOSE_S, 5).

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

callback_mode() -> [handle_event_function, state_enter].

terminate(_Reason, State, _Data) ->
    State =/= locked andalso do_lock(terminate),
    ok.

%% State: locked
handle_event(enter, OldState, locked, Data) ->
    do_lock(OldState),
    {keep_state, Data#{buttons := []}};
handle_event(state_timeout, button, locked, Data) ->
    io:format("Button sequence reset buttons=~p~n", [maps:get(buttons, Data)]),
    {keep_state, Data#{buttons := []}};
handle_event(_EventType = internal,
             _EventContent = {button, Button},
             _State = locked,
             #{code := Code, length := Length, buttons := Buttons} = Data) ->
    NewButtons = case length(Buttons) of
                     Length -> tl(Buttons);
                     _ -> Buttons
                 end ++ [Button],
    case NewButtons of
        Code ->
            {next_state, open, Data};
        _Other ->
            {keep_state, Data#{buttons := NewButtons},
             [{state_timeout, ?BUTTON_TIMEOUT_S*1000, button}]}
    end;
%% State: open
handle_event(enter, OldState, open, Data) ->
    do_unlock(OldState),
    {keep_state_and_data, [{state_timeout, ?CLOSE_S*1000, lock}]};
handle_event(state_timeout, lock, open, Data) ->
    {next_state, locked, Data};
handle_event(internal, {button, _}, open, _) ->
    {keep_state_and_data, [postpone]};
%% Common
handle_event({call, From}, code_length, _, #{length := Length}) ->
    {keep_state_and_data, {reply, From, Length}};
handle_event(cast, {down, Button}, _, Data) ->
    {keep_state, Data#{button => Button}};
handle_event(cast, {up, Button}, _, Data) ->
    case Data of
        #{button := Button} ->
            {keep_state, maps:remove(button, Data),
             [{next_event, internal, {button, Button}}]};
        #{} ->
            keep_state_and_data
    end.


%% Internal Functions

do_unlock(OldState) ->
    io:format("The door has been unlocked after state=~p..."
              "Will get locked in ~p s~n", [OldState, ?CLOSE_S]).

do_lock(OldState) ->
    io:format("The door has been locked after state=~p... ~n", [OldState]).
