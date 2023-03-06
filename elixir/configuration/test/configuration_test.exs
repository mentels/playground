defmodule ConfigurationTest do
  use ExUnit.Case
  doctest Configuration

  test "greets the world" do
    assert Configuration.hello() == :world
  end
end
