defmodule ExPbt do
  @moduledoc """
  Documentation for ExPbt.
  """

  @doc """
  Example invocation of StreamData.check_all/3
  """
  def sample_check_all do
    opts = [initial_seed: :os.timestamp()] #max_shirinking_steps: 100]
    {:error, metadata} =
      StreamData.check_all(StreamData.integer(), opts, fn int ->
        if int == 0 or rem(int, 11) != 0 do
          {:ok, nil}
        else
          {:error, Integer.to_string(int)}
        end
      end)
    metadata
  end

end
