ExUnit.start

defmodule MyServer do
  use GenServer, restart: :temporary
  def start_link([]), do: GenServer.start_link(__MODULE__, [])
  def init(args) do
    IO.puts "Initializing #{inspect self()} ..."
    true = Process.register(self(), __MODULE__)
    {:ok, args}
  end
end

defmodule DisableLogging do
  use ExUnit.Case

  setup do
    Process.flag(:trap_exit, true)
    :ok
  end

  test "Logger will log gen_server's crash" do
    {:ok, _spid} = Supervisor.start_link([MyServer], [strategy: :one_for_one])
    ppid = Process.whereis(MyServer)
    Process.link(ppid)
    Process.exit(ppid, :bye)
    assert_receive {:EXIT, ^ppid, :bye}
    Process.sleep(500)
  end
end
