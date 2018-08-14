#!/usr/bin/env escript
main(_) ->
    %% code:load_file(door_lock).
    [code:load_file(
       list_to_atom(
         filename:rootname(
           filename:basename(F)
          )
        )
      ) || F <- filelib:wildcard("src/door_lock*.erl")].
