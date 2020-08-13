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

defmodule KeepLocation do
  defmacro my_macro(opts) do
    quote location: :keep do
      IO.inspect(opts)
    end
  end
end

defmodule MyModule do
  require BindQuoted

  BindQuoted.my_macro(expr: DateTime.utc_now(), bind_quoted: true)
  BindQuoted.my_macro(expr: DateTime.utc_now(), bind_quoted: false)

  require KeepLocation
  # errors pointing to code in KeepLocation module due to the location: :keep
  # ** (CompileError) quote.exs:27: undefined function opts/0
  #   (stdlib 3.12) lists.erl:1354: :lists.mapfoldl/3
  #   expanding macro: KeepLocation.my_macro/1
  #   quote.exs:39: MyModule (module)
  # KeepLocation.my_macro(hello: :world)
end
