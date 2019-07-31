-module(assertions).

-include_lib("eunit/include/eunit.hrl").

assertion_test() ->
    ?assertError({my_error, _}, my_error()).

assertion_gurad_test() ->
    ?assertError({my_error, Attrs} when hd(Attrs) =:= {attr, val}, my_error()).

assertion_gurad2_test() ->
    ExpectedAttr = {attr, val},
    ?assertError({my_error, Attrs} when hd(Attrs) =:= ExpectedAttr, my_error()).

my_error() ->
    erlang:error({my_error, [
                             {attr, val}
                            ]}).
