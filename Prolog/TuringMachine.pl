% Define the transition rules
transition(q0, '1', q1, '1', right).
transition(q0, '0', q2, '1', right).
transition(q1, '1', q0, '1', right).
transition(q1, '0', q1, '1', right).
transition(q2, '1', q2, '1', right).
transition(q2, '0', halt, '0', stay).

% Move the head in the specified direction
move_head(right, Head, NewHead) :- NewHead is Head + 1.
move_head(left, Head, NewHead) :- NewHead is Head - 1.
move_head(stay, Head, Head).

% Update the tape at the specified position
update_tape(Tape, Head, Symbol, NewTape) :-
    nth0(Head, Tape, _, Rest),
    nth0(Head, NewTape, Symbol, Rest).

% Run the Turing machine
run_machine(State, Tape, Head) :-
    ( State = halt ->
        writeln('Final Tape: '), writeln(Tape)
    ; nth0(Head, Tape, Symbol, _Rest) ->
        transition(State, Symbol, NewState, WriteSymbol, Direction),
        update_tape(Tape, Head, WriteSymbol, NewTape),
        move_head(Direction, Head, NewHead),
        writeln(['State:', State, 'Tape:', NewTape, 'Head:', NewHead]),
        run_machine(NewState, NewTape, NewHead)
    ; append(Tape, ['0'], NewTape),
        run_machine(State, NewTape, Head)
    ).

% Initial setup
turing_machine :-
    InitialTape = ['1', '1', '0', '1'],
    InitialHead = 0,
    InitialState = q0,
    run_machine(InitialState, InitialTape, InitialHead).
