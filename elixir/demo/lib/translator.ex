defmodule Translator do

  defmodule State do
    defstruct [:dict, :wordFreq, :count, :unknown]
  end

  def start() do
    start(%{"hello" => "hola", "world" => "mundo"})
  end

  def start(dict) do
    spawn(fn -> loop(%State{dict: dict, wordFreq: %{}, count: 0}) end)
  end

  def loop(state) do

    receive do

      {:translate, from, doc} ->

        words = doc
          |> String.downcase
          |> String.split(" ")

        translation = words
          |> Enum.map(fn w -> Map.get(state.dict, w, w <> "?") end)
          |> Enum.join(" ")

        send(from, {:translation, translation})

        freq = Enum.frequencies(words)

        newFreq = Map.merge(state.wordFreq, freq, fn _k, v1, v2 -> v1 + v2 end)

        loop(%State{state | wordFreq: newFreq, count: state.count + 1})

      {:stats, from} ->
        send(from, {:stats, state.wordFreq})
        loop(state)

      {:reset, from} ->
        send(from, {:ok, state.wordFreq})
        loop(%State{state | wordFreq: %{}, count: 0})

      other ->
        IO.puts("Error...")
        IO.inspect(other)
        loop(state)
    end

  end
end
