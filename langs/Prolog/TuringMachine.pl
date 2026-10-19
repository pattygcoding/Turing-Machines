:- use_module(library(readutil)).

is_bit('0').
is_bit('1').

all_bits([]).
all_bits([Character | Rest]) :-
    is_bit(Character),
    all_bits(Rest).

% The canonical machine: a binary incrementer.  Reading the tape from the
% right, every 1 becomes a 0 (carry); the first 0 becomes a 1 and the carry
% stops; if the carry runs off the tape a new leading 1 is added.
increment([], ['1']).
increment(['1' | Rest], ['0' | Result]) :-
    increment(Rest, Result).
increment([Character | Rest], ['1' | Rest]) :-
    Character \= '1'.

main :-
    read_line_to_string(user_input, Raw),
    ( Raw == end_of_file -> Line = "" ; Line = Raw ),
    string_chars(Line, Characters),
    ( all_bits(Characters) ->
        reverse(Characters, Reversed),
        increment(Reversed, Incremented),
        reverse(Incremented, Result),
        format("Initial tape: ~s~n", [Line]),
        format("Final tape: ~s~n", [Result])
    ; format("Invalid input: expected a binary string.~n")
    ).
