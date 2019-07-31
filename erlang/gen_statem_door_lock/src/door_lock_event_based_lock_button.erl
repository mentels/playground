-module(door_lock_event_based_lock_button).
-behvaour(gen_statem).

-export([start_link/2]).
-export([button/1]).
-export([code_length/0]).
-export([set_lock_button/1]).
-export([init/1,callback_mode/0,terminate/3]).
-export([handle_event/4]).

-define(BUTTON_TIMEOUT_S, 5).
-define(CLOSE_S, 5).

%%% API

start_link(Code, LockButton) when is_list(Code)  ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [Code, LockButton], []).

down(Button) when is_integer(Button) ->
    gen_statem:cast(?MODULE, {down, Button}).

up(Button) when is_integer(Button) ->
    gen_statem:cast(?MODULE, {up, Button}).

code_length() ->
    gen_statem:call(?MODULE, code_length).

set_lock_button(LockButton) ->
    gen_statem:call(?MODULE, {set_lock_button, LockButton}).

%%% Tests Helpers

button(Button) when is_integer(Button) ->
    down(Button),
    up(Button).

%% Behaviour Callbacks

init([Code, LockButton]) ->
    {ok, {locked, LockButton},
     #{code => Code, length => length(Code), buttons => []}}.

callback_mode() -> [handle_event_function, state_enter].

terminate(_Reason, {State, _}, _Data) ->
    State =/= locked andalso do_lock(terminate),
    ok.

%% State: locked
handle_event(enter, OldState, {locked, _}, Data) ->
    do_lock(OldState),
    {keep_state, Data#{buttons := []}};
handle_event(state_timeout, button, {locked, _}, Data) ->
    do_reset_buttons(maps:get(buttons, Data)),
    {keep_state, Data#{buttons := []}};
handle_event(_EventType = internal,
             _EventContent = {button, Button},
             _State = {locked, LockButton},
             #{code := Code, length := Length, buttons := Buttons} = Data) ->
    NewButtons = case length(Buttons) of
                     Length -> tl(Buttons);
                     _ -> Buttons
                 end ++ [Button],
    case NewButtons of
        Code ->
            {next_state, {open, LockButton}, Data};
        _Other ->
            {keep_state, Data#{buttons := NewButtons},
             [{state_timeout, ?BUTTON_TIMEOUT_S*1000, button}]}
    end;
%% State: open
handle_event(enter, OldState, {open, _}, _Data) ->
    do_unlock(OldState),
    {keep_state_and_data, [{state_timeout, ?CLOSE_S*1000, lock}]};
handle_event(state_timeout, lock, {open, LockButton}, Data) ->
    {next_state, {locked, LockButton}, Data};
handle_event(internal, {button, Button}, {open, LockButton}, Data)
  when Button =:= LockButton ->
    {next_state, {locked, LockButton}, Data};
handle_event(internal, {button, _}, {open, _}, _) ->
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
    end;
handle_event(cast, {set_lock_button, Button}, {State, _}, Data) ->
    {next_state, {State, Button}, Data}.


%% Internal Functions

do_unlock(OldState) ->
    io:format("[~s] The door has been unlocked after state=~p..."
              "Will get locked in ~p s~n", [timestamp(), OldState, ?CLOSE_S]).

do_lock(OldState) ->
    io:format("[~s] The door has been locked after state=~p... ~n",
              [timestamp(), OldState]).

do_reset_buttons(Buttons) ->
    io:format("[~s] Button sequence reset buttons=~p~n", [timestamp(), Buttons]).


timestamp() ->
    {{Y,M,D},{H,MM,S}}=calendar:local_time(),
    FmtStr = "~B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B",
    io_lib:format(FmtStr, [Y,M,D,H,MM,S]).
