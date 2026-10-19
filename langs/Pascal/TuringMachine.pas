program TuringMachine;

var
    line: string;
    tape: string;
    index: Integer;
    carry: Boolean;

begin
    ReadLn(line);

    { Drop a trailing carriage return if the platform kept one. }
    if (Length(line) > 0) and (line[Length(line)] = #13) then
        Delete(line, Length(line), 1);

    for index := 1 to Length(line) do
        if (line[index] <> '0') and (line[index] <> '1') then
        begin
            WriteLn('Invalid input: expected a binary string.');
            Halt(0);
        end;

    { The canonical machine: a binary incrementer.  Reading the tape from the
      right, every 1 becomes a 0 (carry); the first 0 becomes a 1 and the carry
      stops; if the carry runs off the tape a new leading 1 is added. }
    tape := line;
    carry := True;
    index := Length(tape);
    while carry and (index >= 1) do
    begin
        if tape[index] = '1' then
        begin
            tape[index] := '0';
            Dec(index);
        end
        else
        begin
            tape[index] := '1';
            carry := False;
        end;
    end;
    if carry then
        tape := '1' + tape;

    WriteLn('Initial tape: ', line);
    WriteLn('Final tape: ', tape);
end.
