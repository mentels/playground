-module(mynif).
%% the module implements the same functions as the NIF C module
%% the module with NIF cannot be reload; the way to do it:
%% 1. unload the module (code:purge will automatically unload)
%% 3. load again
%% init cannot be called twice as SO lib cannot be loaded twice

%% with this calling init/0 is not necessary
-on_load(init/0).

-export([init/0, hello_world/0,
         counter_inc/0, counter_get/0, counter_add/1,
         new/0, resize/2, shuffle/1]).

init() ->
    %% 0 ends up in load_info of NIF load callback
    erlang:load_nif("priv/mynif", 0).

hello_world() ->
    %% the same as erlang:error but dialyzer ignores it
    erlang:nif_error(nif_not_loaded).

counter_get() ->
    erlang:nif_error(nif_not_loaded).

counter_inc() ->
    erlang:nif_error(nif_not_loaded).

counter_add(_Num) ->
    erlang:nif_error(nif_not_loaded).

new() ->
    erlang:nif_error(nif_not_loaded).

resize(_Data, _NewSize) ->
    erlang:nif_error(nif_not_loaded).

shuffle(_Data) ->
    erlang:nif_error(nif_not_loaded).
