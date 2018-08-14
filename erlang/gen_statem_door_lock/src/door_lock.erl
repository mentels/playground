-module(door_lock).

%% Correct code
-export([test_correct_code/1]).
-export([test_correct_code_with_locking/1]).
%% Incorrect code
-export([test_incorrect_code/1]).
%% Correct then incorrect
-export([test_incorrect_then_correct_code/1]).
%% Code length
-export([test_code_length/1]).
%% Buttons reset
-export([test_code_sequence_resetting/1]).
%% Buttons postponed
-export([test_buttons_send_when_doors_open_are_retried/1]).

%% Correct code

test_correct_code(Backend) when
      Backend == door_lock_state_based;
      Backend == door_lock_event_based ->
    stop_wrapper(fun() ->
                         Backend:start_link([1,2,3]),
                         [Backend:button(B) || B <- [1,2,3]]
                 end,
                 Backend);
test_correct_code(Backend) when Backend == door_lock_event_based_lock_button ->
    stop_wrapper(fun() ->
                         Backend:start_link([1,2,3], 9),
                         [Backend:button(B) || B <- [1,2,3]]
                 end, Backend).

test_correct_code_with_locking(Backend)
  when Backend == door_lock_event_based_lock_button ->
    stop_wrapper(fun() ->
                         Backend:start_link([1,2,3], 9),
                         [Backend:button(B) || B <- [1,2,3]],
                         Backend:button(9)
                 end, Backend).

%% Incorrect code

test_incorrect_code(Backend) when
      Backend == door_lock_state_based;
      Backend == door_lock_event_based ->
    Backend:start_link([1,2,3]),
    [Backend:button(B) || B <- [0,1,2]];
test_incorrect_code(Backend)
  when Backend == door_lock_event_based_lock_button ->
    Backend:start_link([1,2,3], 9),
    [Backend:button(B) || B <- [0,1,2]].

%% Correct then incorrect

test_incorrect_then_correct_code(Backend) when
      Backend == door_lock_state_based;
      Backend == door_lock_event_based ->
    stop_wrapper(fun() ->
                         Backend:start_link([1,2,3]),
                         [Backend:button(B) || B <- [0,1,2,3]]
                 end, Backend);
test_incorrect_then_correct_code(Backend)
  when Backend == door_lock_event_based_lock_button ->
    stop_wrapper(fun() ->
                         Backend:start_link([1,2,3], 9),
                         [Backend:button(B) || B <- [0,1,2,3]]
                 end, Backend).

%% Code length

test_code_length(Backend) when
      Backend == door_lock_state_based;
      Backend == door_lock_event_based ->
    {ok, _Pid} = Backend:start_link([1,2,3]),
    io:format("Code length before sending buttons: ~p~n", [Backend:code_length()]),
    [Backend:button(B) || B <- [1,2]],
    io:format("Code length after sending 2 correct buttons: ~p~n", [Backend:code_length()]),
    Backend:button(3),
    io:format("Code length after sending 3rd correct button: ~p~n", [Backend:code_length()]),
    [Backend:button(B) || B <- [0,2,3]],
    io:format("Code length after sending incorrect buttons: ~p~n", [Backend:code_length()]),
    %% will lock the door when stopping
    gen_statem:stop(Backend);
test_code_length(Backend)
  when Backend == door_lock_event_based_lock_button ->
    {ok, _Pid} = Backend:start_link([1,2,3], 9),
    io:format("Code length before sending buttons: ~p~n", [Backend:code_length()]),
    [Backend:button(B) || B <- [1,2]],
    io:format("Code length after sending 2 correct buttons: ~p~n", [Backend:code_length()]),
    Backend:button(3),
    io:format("Code length after sending 3rd correct button: ~p~n", [Backend:code_length()]),
    [Backend:button(B) || B <- [0,2,3]],
    io:format("Code length after sending incorrect buttons: ~p~n", [Backend:code_length()]),
    %% will lock the door when stopping
    gen_statem:stop(Backend).

%% Buttons resetting

test_code_sequence_resetting(Backend) when
      Backend == door_lock_state_based;
      Backend == door_lock_event_based ->
    {ok, _Pid} = Backend:start_link([1,2,3]),
    io:format("Send two correct buttons and wait for 6s~n", []),
    [Backend:button(B) || B <- [1,2]],
    timer:sleep(6*1000),
    io:format("Send the last correct button~n", []),
    Backend:button(3);
test_code_sequence_resetting(Backend)
  when Backend == door_lock_event_based_lock_button ->
    {ok, _Pid} = Backend:start_link([1,2,3], 9),
    io:format("Send two correct buttons and wait for 6s~n", []),
    [Backend:button(B) || B <- [1,2]],
    timer:sleep(6*1000),
    io:format("Send the last correct button~n", []),
    Backend:button(3).

%% Send correct buttons while doors open

test_buttons_send_when_doors_open_are_retried(Backend) when
      Backend == door_lock_state_based;
      Backend == door_lock_event_based ->
    stop_wrapper(fun() ->
                         Backend:start_link([1,2,3]),
                         [Backend:button(B) || B <- [1,2,3]],
                         [Backend:button(B) || B <- [1,2,3]],
                         io:format("The correct buttons has been sent while the doors are open~n")
                 end, Backend);
test_buttons_send_when_doors_open_are_retried(Backend)
  when Backend == door_lock_event_based_lock_button ->
    stop_wrapper(fun() ->
                         Backend:start_link([1,2,3], 9),
                         [Backend:button(B) || B <- [1,2,3]],
                         [Backend:button(B) || B <- [1,2,3]],
                         io:format("The correct buttons has been sent while the doors are open~n")
                 end, Backend).


stop_wrapper(Fun, Backend) ->
    spawn_link(
      fun() ->
              io:format("Send me (~p) 'stop' to stop the door_lock", [self()]),
              Fun(),
              receive stop -> gen_statem:stop(Backend) end
      end).
