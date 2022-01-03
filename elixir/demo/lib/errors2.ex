defmodule Errors2 do

  def f(n) do
    c = 10 / n
    IO.puts("Running step #{n} - result #{c}")
    Process.sleep(2000)
    f(n - 1)
  end

  def start_f(n) do
    spawn fn -> f(n) end
  end

  def start_link_f(n) do
    spawn_link fn -> f(n) end
  end

  def start_link_2_f(n) do
    spawn(
      fn ->
        Process.flag(:trap_exit, true)
        spawn_link fn -> f(n) end
        receive do
          {:EXIT, _from, _reason}  ->
            IO.puts("Exit detected")
        end

      end
    )
  end

  def on_exit(process_id, func) do
    spawn(
      fn ->
        Process.flag(:trap_exit, true)
        Process.link(process_id)

        receive do
          {:EXIT, ^process_id, reason} -> # ^ no cambia el valor
            func.(reason)
        end

      end
    )
  end

  def test_on_exit() do
    p = start_f(5)
    on_exit(p, fn _reason -> IO.puts("Process ended ...") end)
  end

  def keep_alive(name, func) do
    pid = spawn(func)
    Process.register(pid, name)
    on_exit(pid, fn _reason -> keep_alive(name, func) end)
  end

  def keep_alive_test do
    keep_alive(:my_process, fn -> f(5) end)
  end
end
