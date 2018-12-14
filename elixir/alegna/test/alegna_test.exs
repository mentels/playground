defmodule AlegnaTest do
  use ExUnit.Case
  doctest Alegna

  test "greets the world" do
    assert Alegna.hello() == :world
  end
end
