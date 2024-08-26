-module(turing_machine).
-export([execute/1]).

-record(tm, {tape, head = 2, state = q0}).

new(Input) ->
    #tm{tape = lists:concat(["  ", Input, "  "])}.

execute(#tm{tape = Tape, head = Head, state = State} = TM) ->
    case State of
        accept -> io:format("Input is accepted by the Turing Machine.~n");
        reject -> io:format("Input is rejected by the Turing Machine.~n");
        _ ->
            CurrentSymbol = lists:nth(Head, Tape),
            TM1 = case {State, CurrentSymbol} of
                {q0, $a} ->
                    TM#tm{tape = set_element(Head, Tape, $X), head = Head + 1, state = q1};
                {q0, $\s} -> TM#tm{state = accept};
                {q1, $a} -> TM#tm{head = Head + 1};
                {q1, $X} -> TM#tm{head = Head + 1};
                {q1, $b} ->
                    TM#tm{tape = set_element(Head, Tape, $Y), head = Head + 1, state = q2};
                {q2, $b} -> TM#tm{head = Head + 1};
                {q2, $Y} -> TM#tm{head = Head + 1};
                {q2, $\s} -> TM#tm{state = accept};
                _ -> TM#tm{state = reject}
            end,
            execute(TM1)
    end.
