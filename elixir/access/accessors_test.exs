ExUnit.start

defmodule AccessorsTest do
  @moduledoc """
  Tests for learning Elixir Accessors
  """

  use ExUnit.Case

  setup do
    family = %{woman: %{name: "alice", age: 29},
               man: %{name: "szymon", age: 26},
               child: nil}
    {:ok, as_map: family, as_kw: Keyword.new(family)}
  end

  ### get_in (function)

  test "Get one name", %{as_map: map, as_kw: kw} do
    fun = fn family -> 
      assert "alice" ==  get_in(family, [:woman, :name])
      assert nil == get_in(family, [:child, :name])
      assert nil == get_in(family, [:non_existent, :name])
    end
    for f <- [map, kw], do: fun.(f)
  end

  test "Get all the names", %{as_map: map, as_kw: kw} do
    fun = fn(:get, collection, next_fn) when is_list(collection) ->
      for {_, data} <- collection, do: next_fn.(data)
    (:get, collection, next_fn) when is_map(collection) ->
      for data <- Map.values(collection), do: next_fn.(data)
    end
    assert [nil, "szymon", "alice"] == get_in(kw, [fun, :name])
    assert [nil, "szymon", "alice"] == get_in(map, [fun, :name])
  end

  ### put_in

  test "Change age for map", %{as_map: map} do
    # macros
    assert 33 == (put_in(map.woman.age, 33))[:woman][:age]
    assert 33 == (put_in(map[:woman].age, 33))[:woman][:age]
    assert 33 == (put_in(map[:woman][:age], 33))[:woman][:age]
    # function
    assert 33 == (put_in(map, [:woman, :age], 33))[:woman][:age]
  end

  ### TODO START
  
  test "Change ages for map", %{as_map: map} do
    # function
    fun = fn(:get_and_update, collection, next_fn) ->
      c = collection
      |> Enum.filter_map(fn {_, v} -> v end, fn {_, v} -> next_fn.(v) end)
      |> Keyword.values
      IO.puts inspect c
    end
    map = put_in(map, [fun, :age], 40)
    assert {40, 40} = {map.man.age, map.woman.age}
    assert_raise BadKeyError, fn ->  map.child.age end
  end

  ### TODO END

  test "Change age for kw", %{as_kw: kw} do
    # macros
    assert_raise BadMapError, fn -> put_in(kw.woman.age, 33) end
    assert 33 == (put_in(kw[:woman].age, 33))[:woman][:age]
    assert 33 == (put_in(kw[:woman][:age], 33))[:woman][:age]
    # function
    assert 33 == (put_in(kw, [:woman, :age], 33))[:woman][:age]
  end

  ### TODO

  #### get_and_update_in
  #### update_in
  

end
