with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure TuringMachine is
   Line  : Unbounded_String;
   Tape  : Unbounded_String;
   Head  : Natural;
   Carry : Boolean := True;
begin
   begin
      Line := To_Unbounded_String (Get_Line);
   exception
      when End_Error =>
         Line := Null_Unbounded_String;
   end;

   declare
      Text : constant String := To_String (Line);
   begin
      for Index in Text'Range loop
         if Text (Index) /= '0' and Text (Index) /= '1' then
            Put_Line ("Invalid input: expected a binary string.");
            return;
         end if;
      end loop;
   end;

   --  The canonical machine: a binary incrementer.  Reading the tape from the
   --  right, every 1 becomes a 0 (carry); the first 0 becomes a 1 and the carry
   --  stops; if the carry runs off the tape a new leading 1 is added.
   Tape := Line;
   if Length (Tape) = 0 then
      Tape := To_Unbounded_String ("1");
   else
      Head := Length (Tape);
      while Carry and Head >= 1 loop
         if Element (Tape, Head) = '1' then
            Replace_Element (Tape, Head, '0');
            Head := Head - 1;
         else
            Replace_Element (Tape, Head, '1');
            Carry := False;
         end if;
      end loop;
      if Carry then
         Tape := To_Unbounded_String ("1") & Tape;
      end if;
   end if;

   Put_Line ("Initial tape: " & To_String (Line));
   Put_Line ("Final tape: " & To_String (Tape));
end TuringMachine;
