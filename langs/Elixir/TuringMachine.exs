defmodule TuringMachine do
  defstruct tape: [], head: 0, state: :q0

  def new(input) do
    %TuringMachine{
      tape: String.graphemes("  " <> input <> "  "),
      head: 2,
      state: :q0
    }
  end

  def execute(%TuringMachine{tape: tape, head: head, state: state} = tm) do
    case state do
      :accept ->
        IO.puts("Input is accepted by the Turing Machine.")
      :reject ->
        IO.puts("Input is rejected by the Turing Machine.")
      _ ->
        current_symbol = Enum.at(tape, head)
        tm = case {state, current_symbol} do
          {:q0, "a"} ->
            %TuringMachine{tm | tape: List.replace_at(tape, head, "X"), head: head + 1, state: :q1}
          {:q0, " "} ->
            %TuringMachine{tm | state: :accept}
          {:q1, "a"} ->
            %TuringMachine{tm | head: head + 1}
          {:q1, "X"} ->
            %TuringMachine{tm | head: head + 1}
          {:q1, "b"} ->
            %TuringMachine{tm | tape: List.replace_at(tape, head, "Y"), head: head + 1, state: :q2}
          {:q2, "b"} ->
            %TuringMachine{tm | head: head + 1}
          {:q2, "Y"} ->
            %TuringMachine{tm | head: head + 1}
          {:q2, " "} ->
            %TuringMachine{tm | state: :accept}
          _ ->
            %TuringMachine{tm | state: :reject}
        end
        execute(tm)
    end
  end
end

input = "aabb"
TuringMachine.new(input) |> TuringMachine.execute()
