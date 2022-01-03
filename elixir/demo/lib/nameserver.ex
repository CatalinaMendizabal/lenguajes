defmodule NameServer do

  def add(host, ip) do
    send(:ns, {:add, self(), {host, ip}})
  end

  def lookup(host) do
    send(:ns, {:lookup, self(), {host}})

    receive do
      {:ip, value} ->
        value
    end
  end

  def start do
    pid = spawn(__MODULE__, :loop, [init()]) # __MODUE__ es lo mismo que poner NameServer
    Process.register(pid, :ns) # :ns le puse un nombre especifio
    pid
  end

  def init() do
    %{}
  end

  def loop(map) do
    receive do

      {:add, _from, {host, ip}} ->
        new_map = Map.put(map, host, ip)
        loop(new_map)

      {:lookup, from, {host}} ->
        ip = Map.get(map, host, :not_found)
        send from, {:ip, ip}
        loop(map)

    end

  end
end
