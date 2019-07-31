defmodule MapLiterals do
  fields = for i <- 1..15, do: :"field#{i}"
  defstruct fields

  value = quote(do: value)
  field_values = Enum.zip(fields, Stream.cycle([value]))

  def new(unquote(value)) do
    %__MODULE__{unquote_splicing(field_values)}
  end
end

Benchee.run(%{"new" => fn -> MapLiterals.new(1) end}, time: 30)
