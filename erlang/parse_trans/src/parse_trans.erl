-module(parse_trans).

%% API exports
-export([]).

%%====================================================================
%% API functions
%%====================================================================

%%% @doc Returns the AST of a file
%%%
%%% It won't apply any of the parse transforms
file_ast(Filename) ->
    epp:parse_file(Filename, []).

%%% @doc Returns the AST of a file with `ParseTrans' applied.
print_file_ast_after_parse_trans(Filename, ParseTrans) ->
    {ok, AST} = file_ast(Filename),
    ParseTrans:parse_transform(AST, []).

%%====================================================================
%% Internal functions
%%====================================================================
