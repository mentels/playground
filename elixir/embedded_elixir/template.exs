defmodule Templates do
  def template() do
    {:ok, template} = File.read("template.erl")
    template
  end

  def templated_with_vars() do
    """
    -module(mod).

    -export([init/0, start/1]).

    -required_variable({var1,<<"description for var1">>}).

    -required_variable({var2,<<"description for var2">>}).

    init() -> ok.
    start(_) -> ok.
    """
  end

  def templated_without_vars() do
    """
    -module(mod).

    -export([init/0, start/1]).

    init() -> ok.
    start(_) -> ok.
    """
  end
end

with_vars = %{module_name: :mod,
              required_vars: %{
                var1: "description for var1",
                var2: "description for var2",
              }}
without_vars = %{with_vars | required_vars: %{}}
do_template = fn(t, vars) -> EEx.eval_string(t, Map.to_list(vars)) end

import ExUnit.Assertions
tmpl = Templates.template()
assert Templates.templated_with_vars() == do_template.(tmpl, with_vars)
assert Templates.templated_without_vars() == do_template.(tmpl, without_vars)
