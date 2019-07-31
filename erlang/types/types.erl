-module(types).

-compile(export_all).

-type some() :: _.

-spec test() -> some().
test() ->
     ok.

-spec empty_map() -> #{}.
empty_map() ->
    #{ala => 11}.

call() ->
     empty_map().
