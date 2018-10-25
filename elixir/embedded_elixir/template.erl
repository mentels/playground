-module(<%= module_name %>).

-export([init/0, start/1]).
<%= for variable <- required_vars do %>
-required_variable(<%= :io_lib.format("~p", [variable]) %>).
<% end %>
init() -> ok.
start(_) -> ok.
