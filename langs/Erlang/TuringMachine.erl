-module('TuringMachine').

-export([main/0]).

main() ->
    Line = read_line(),
    case lists:any(fun(Character) -> Character =/= $0 andalso Character =/= $1 end, Line) of
        true ->
            io:format("Invalid input: expected a binary string.~n");
        false ->
            Result = lists:reverse(increment(lists:reverse(Line))),
            io:format("Initial tape: ~s~n", [Line]),
            io:format("Final tape: ~s~n", [Result])
    end.

read_line() ->
    case io:get_line("") of
        eof -> "";
        {error, _} -> "";
        Data -> string:trim(Data, trailing, "\r\n")
    end.

%% The canonical machine: a binary incrementer.  Reading the tape from the
%% right, every 1 becomes a 0 (carry); the first 0 becomes a 1 and the carry
%% stops; if the carry runs off the tape a new leading 1 is added.
increment([]) -> [$1];
increment([$1 | Rest]) -> [$0 | increment(Rest)];
increment([_ | Rest]) -> [$1 | Rest].
