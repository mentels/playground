-module(xref_sut).
-export([exp/1]).

exp(A) ->
    xref_sut:z_undef(),
    xref_sut:unexp(A).
unexp(_) ->
    true.
