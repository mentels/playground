-module(dummy_mod).
-compile([export_all]).

f() -> receive stop -> ok end.
