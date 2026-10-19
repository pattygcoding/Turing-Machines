defmodule TuringMachine do
  @moduledoc false

  # The canonical machine: a binary incrementer.  Reading the tape from the
  # right, every 1 becomes a 0 (carry); the first 0 becomes a 1 and the carry
  # stops; if the carry runs off the tape a new leading 1 is added.
  def increment([]), do: ["1"]
  def increment(["1" | rest]), do: ["0" | increment(rest)]
  def increment([_ | rest]), do: ["1" | rest]
end

line =
  case IO.read(:line) do
    :eof -> ""
    {:error, _} -> ""
    data -> data |> String.trim_trailing("\n") |> String.trim_trailing("\r")
  end

if String.match?(line, ~r/[^01]/) do
  IO.puts("Invalid input: expected a binary string.")
else
  result =
    line
    |> String.graphemes()
    |> Enum.reverse()
    |> TuringMachine.increment()
    |> Enum.reverse()
    |> Enum.join()

  IO.puts("Initial tape: " <> line)
  IO.puts("Final tape: " <> result)
end
