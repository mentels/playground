defmodule BindQuoted do
  # the macro receives an AST
  defmacro my_macro(opts) do
    if opts[:bind_quoted] do
      IO.inspect(opts[:expr], label: "OUTSIDE QUOTE")

      quote bind_quoted: [expression: opts[:expr]], unquote: true do
        IO.inspect(expression, label: "EXPR WITH BIND_QUOTED 1")
        IO.inspect(expression, label: "EXPR WITH BIND_QUOTED 2")
        # will error with:
        # ** (CompileError) bind_quoted.exs:24: unquote called outside quote
        # unless unqoute: true is set
        IO.inspect(unquote(opts[:expr]), label: "EXPR WITH BIND_QUOTED 3")
      end
    else
      quote do
        IO.inspect(unquote(opts[:expr]), label: "EXPR W/O BIND_QUOTED 1")
        IO.inspect(unquote(opts[:expr]), label: "EXPR W/O BIND_QUOTED 2")
      end
    end
  end
end

defmodule MyModule do
  require BindQuoted

  BindQuoted.my_macro(expr: DateTime.utc_now(), bind_quoted: true)
  BindQuoted.my_macro(expr: DateTime.utc_now(), bind_quoted: false)
end
