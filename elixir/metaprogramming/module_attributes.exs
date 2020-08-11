defmodule PutAttribute do
  Module.put_attribute(__MODULE__, :attr, :put1)
  Module.put_attribute(__MODULE__, :attr, :put2)

  def foo, do: @attr
end

IO.inspect(PutAttribute.foo(),
  label: "PutAttribute.foo()"
)

defmodule RegisterAndPutAttribute do
  Module.register_attribute(__MODULE__, :attr, [])
  Module.put_attribute(__MODULE__, :attr, :put1)
  Module.put_attribute(__MODULE__, :attr, :put2)

  def foo, do: @attr
end

IO.inspect(RegisterAndPutAttribute.foo(),
  label: "RegisterAndPutAttribute.foo()"
)

defmodule RegisterAccumulateAndPutAttribute do
  Module.register_attribute(__MODULE__, :attr, accumulate: true)
  Module.put_attribute(__MODULE__, :attr, :put1)
  Module.put_attribute(__MODULE__, :attr, :put2)

  def foo, do: @attr
end

IO.inspect(RegisterAccumulateAndPutAttribute.foo(),
  label: "RegisterAccumulateAndPutAttribute.foo()"
)

defmodule RegisterPutAndDeleteAttribute do
  Module.register_attribute(__MODULE__, :attr, accumulate: true)
  Module.put_attribute(__MODULE__, :attr, :put1)
  Module.put_attribute(__MODULE__, :attr, :put2)
  Module.delete_attribute(__MODULE__, :attr)

  def foo, do: @attr
end

IO.inspect(RegisterPutAndDeleteAttribute.foo(),
  label: "RegisterPutAndDeleteAttribute.foo()"
)
