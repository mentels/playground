# run as elixir  elixir/metaprogramming/eval_string.exs

ExUnit.start()

defmodule MacroModule do
  defmacro __using__(_opts) do
    quote do
      import MacroModule
      @before_compile MacroModule
      Module.register_attribute(__MODULE__, :attr, accumulate: true)
    end
  end

  defmacro __before_compile__(_opts) do
    quote do
      def attr, do: @attr
    end
  end

  defmacro macro(expr) do
    quote bind_quoted: [expr: expr] do
      if is_atom(expr) do
        Module.put_attribute(__MODULE__, :attr, expr)
      else
        raise "macro/1 accepts only atoms"
      end
    end
  end
end

defmodule ModuleUsingMacro do
  use MacroModule

  macro(:ala)
  macro(:ola)
end

IO.inspect(ModuleUsingMacro.attr(), label: "ATTR")

defmodule TestMacro do
  use ExUnit.Case

  test "module compiles" do
    assert Code.eval_string("""
           defmodule ModuleUsingMacro do
           use MacroModule

           macro(:ala)
           macro(:ola)
           end
           """)
  end

  test "attributes are registered" do
    {{:module, module, _, _}, _} =
      Code.eval_string("""
      defmodule ModuleUsingMacro do
      use MacroModule

      macro(:ala)
      macro(:ola)
      end
      """)

    assert [:ola, :ala] = module.attr
  end

  test "comples expression raises" do
    assert_raise RuntimeError, "macro/1 accepts only atoms", fn ->
      Code.eval_string("""
      defmodule ModuleUsingMacro do
      use MacroModule

      macro(:ala)
      macro("ola")
      end
      """)
    end
  end
end
