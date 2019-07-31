-file("simple.erl", 1).

-module(z).

-export([f/0]).

bin() ->
    <<"Some binary value">>.

map() ->
    #{a => 3,b => "Hello"}.

f() ->
    {bin(),map()}.



