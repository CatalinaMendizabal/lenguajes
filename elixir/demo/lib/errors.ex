defmodule Errors do
  defmodule MyError do
    defexception message: "default message"
  end

  def raise_error do
    raise MyError, message: 'my message'
  end

  def raise_and_rescue_error do
    try do
      raise MyError, message: "my message"
    rescue
      e in MyError ->
        IO.puts("Ups ... ")
        e
    end
  end

  def read_file(file_name) do
    case File.read(file_name) do
    {:ok, body} ->
      IO.puts("Success ... #{body}")
    {:error, reason} ->
      IO.puts("Error: #{reason}")
    end
  end

  def throw_any_value do
    try do
      throw(:not_found)
    catch
      v -> IO.puts("error: #{v}")
    end
  end

  def try_after_file do
    {:ok, file} = File.open("file.txt", [:utf8, :write])

    try do
      IO.write(file, "file content")
    after
      File.close(file)
    end
  end
end
