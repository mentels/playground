defmodule ExPbtTest do
  use ExUnit.Case
  use ExUnitProperties

  test "length(list) is always >= 0" do
    check all list <- list_of(term()) do
      assert length(list) >= 0
    end
  end

  @tag :failing
  property "list does not contain multiples of 4" do
    check all list <- list_of(positive_integer()) do
      refute Enum.any?(list, &(rem(&1, 4) == 0))
    end
  end

  # will be reported as property via by ExUnit though the property macro
  property "the in/2 operator works with lists" do
    check all list <- list_of(term()),
              list != [], # filter
              elem <- member_of(list) do # assignment
      assert elem in list
    end
  end

end
