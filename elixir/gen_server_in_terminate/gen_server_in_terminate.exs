# run with:
# elixir gen_server_in_terminate.exs

defmodule SampleGenServer do
  use GenServer

  # the terminate_caller will call the GenServer while it will be terminating
  def start(terminate_caller),
    do: GenServer.start(__MODULE__, terminate_caller, name: __MODULE__)

  def call(), do: GenServer.call(__MODULE__, :call)

  def stop(), do: GenServer.cast(__MODULE__, :stop)

  def init(terminate_caller), do: {:ok, terminate_caller}

  def handle_call(:call, _from, state), do: {:reply, {:ok, :called}, state}

  def handle_cast(:stop, state) do
    IO.puts "[GenServer] Stop casted"
    Process.flag(:trap_exit, true)
    {:stop, :normal_not_really, state}
  end

  def terminate(_, terminate_caller) do
    send(terminate_caller, {self(), :i_am_dying})
    IO.puts "[GenServer] dying"
    receive do
    after 3000 ->
        IO.puts "[GenServer] just died"
        :ok
    end
  end
end

{:ok, pid} = SampleGenServer.start(self())
ref = Process.monitor(pid)
SampleGenServer.stop()
receive do
  {^pid, :i_am_dying} -> :ok
end
IO.puts "[Proc] GenServer informed it's dying. Let's call him!"
try do SampleGenServer.call()
catch :exit, reason ->
    IO.puts "[Proc] calling GenServer failed with reason=#{inspect reason}"
end
receive do
  {:DOWN, ^ref, :process, ^pid, _} ->
    IO.puts "[Proc] got :DOWN message from GenServer"
end
