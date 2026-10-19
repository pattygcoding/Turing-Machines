program TuringMachine;

type
  Direction = (L, R);  { L for Left, R for Right }
  State = string;
  Symbol = char;
  Tape = array of Symbol;
  Transition = record
    WriteSymbol: Symbol;
    MoveDirection: Direction;
    NextState: State;
  end;
  TransitionFunction = array of record
    CurrentState: State;
    ReadSymbol: Symbol;
    Action: Transition;
  end;

var
  tape: Tape;
  headPosition: integer;
  currentState: State;
  transitions: TransitionFunction;

procedure InitializeTape(initialSymbols: string);
var
  i: integer;
begin
  SetLength(tape, Length(initialSymbols));
  for i := 1 to Length(initialSymbols) do
    tape[i-1] := initialSymbols[i];
end;

procedure PrintTape;
var
  i: integer;
begin
  for i := 0 to High(tape) do
    Write(tape[i]);
  Writeln;
end;

function FindTransition(state: State; symbol: Symbol): integer;
var
  i: integer;
begin
  FindTransition := -1;
  for i := 0 to High(transitions) do
    if (transitions[i].CurrentState = state) and (transitions[i].ReadSymbol = symbol) then
    begin
      FindTransition := i;
      Exit;
    end;
end;

procedure Step;
var
  transitionIndex: integer;
  transition: Transition;
begin
  transitionIndex := FindTransition(currentState, tape[headPosition]);
  if transitionIndex = -1 then
  begin
    Writeln('No transition found. Halting.');
    currentState := 'HALT';
    Exit;
  end;

  transition := transitions[transitionIndex].Action;

  { Write the new symbol }
  tape[headPosition] := transition.WriteSymbol;

  { Move the head }
  if transition.MoveDirection = R then
    Inc(headPosition)
  else
    Dec(headPosition);

  { Ensure the tape extends if needed }
  if headPosition < 0 then
  begin
    Insert('_', tape, 0);
    headPosition := 0;
  end
  else if headPosition >= Length(tape) then
    SetLength(tape, Length(tape) + 1);

  { Update the state }
  currentState := transition.NextState;

  { Print current status }
  PrintTape;
  Writeln('Head Position: ', headPosition);
  Writeln('Current State: ', currentState);
  Writeln('---');
end;

procedure RunTuringMachine;
begin
  while currentState <> 'HALT' do
    Step;
end;

begin
  { Initialize tape with the input symbols }
  InitializeTape('101__');

  { Define the transitions }
  SetLength(transitions, 6);
  transitions[0].CurrentState := 'q0';
  transitions[0].ReadSymbol := '1';
  transitions[0].Action := Transition(WriteSymbol: '1', MoveDirection: R, NextState: 'q0');

  transitions[1].CurrentState := 'q0';
  transitions[1].ReadSymbol := '0';
  transitions[1].Action := Transition(WriteSymbol: '0', MoveDirection: R, NextState: 'q1');

  transitions[2].CurrentState := 'q0';
  transitions[2].ReadSymbol := '_';
  transitions[2].Action := Transition(WriteSymbol: '_', MoveDirection: L, NextState: 'HALT');

  transitions[3].CurrentState := 'q1';
  transitions[3].ReadSymbol := '1';
  transitions[3].Action := Transition(WriteSymbol: '0', MoveDirection: L, NextState: 'q1');

  transitions[4].CurrentState := 'q1';
  transitions[4].ReadSymbol := '0';
  transitions[4].Action := Transition(WriteSymbol: '1', MoveDirection: L, NextState: 'q0');

  transitions[5].CurrentState := 'q1';
  transitions[5].ReadSymbol := '_';
  transitions[5].Action := Transition(WriteSymbol: '_', MoveDirection: R, NextState: 'HALT');

  { Initialize the Turing machine }
  headPosition := 0;
  currentState := 'q0';

  { Run the Turing machine }
  RunTuringMachine;
end.
