with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Turing_Machine is

   type Direction is (L, R);
   type State is new String;
   type Symbol is new Character;
   type Tape is array (Integer range <>) of Symbol;

   type Transition_Record is record
      Write_Symbol  : Symbol;
      Move_Direction : Direction;
      Next_State     : State;
   end record;

   type Transition_Array is array (State, Symbol) of Transition_Record;

   subtype Tape_Index is Integer;
   subtype Tape_Length is Integer;

   Tape_Length_Value : constant Tape_Length := 10;
   Tape_Start_Index : constant Tape_Index := 1;
   Tape_End_Index : constant Tape_Index := Tape_Start_Index + Tape_Length_Value - 1;

   Tape_Buffer : Tape(Tape_Start_Index .. Tape_End_Index) := (others => '_');

   Current_State : State := "q0";
   Head_Position : Tape_Index := Tape_Start_Index;

   function Transition_Table return Transition_Array is
   begin
      return (
         ("q0", '1') => (Write_Symbol => '1', Move_Direction => R, Next_State => "q0"),
         ("q0", '0') => (Write_Symbol => '0', Move_Direction => R, Next_State => "q1"),
         ("q0", '_') => (Write_Symbol => '_', Move_Direction => L, Next_State => "HALT"),
         ("q1", '1') => (Write_Symbol => '0', Move_Direction => L, Next_State => "q1"),
         ("q1", '0') => (Write_Symbol => '1', Move_Direction => L, Next_State => "q0"),
         ("q1", '_') => (Write_Symbol => '_', Move_Direction => R, Next_State => "HALT"),
         others => (Write_Symbol => '_', Move_Direction => R, Next_State => "HALT")
      );
   end Transition_Table;

   function Get_Transition(Current_State : State; Current_Symbol : Symbol) return Transition_Record is
      Table : constant Transition_Array := Transition_Table;
   begin
      return Table(Current_State, Current_Symbol);
   end Get_Transition;

   procedure Print_Tape is
   begin
      for Index in Tape_Buffer'Range loop
         Put(Tape_Buffer(Index));
      end loop;
      New_Line;
   end Print_Tape;

   procedure Step is
      Current_Symbol : constant Symbol := Tape_Buffer(Head_Position);
      Transition : constant Transition_Record := Get_Transition(Current_State, Current_Symbol);
   begin
      Tape_Buffer(Head_Position) := Transition.Write_Symbol;

      case Transition.Move_Direction is
         when R => Head_Position := Head_Position + 1;
         when L => Head_Position := Head_Position - 1;
      end case;

      if Head_Position < Tape_Start_Index then
         Head_Position := Tape_Start_Index;
      elsif Head_Position > Tape_End_Index then
         Head_Position := Tape_End_Index;
      end if;

      Current_State := Transition.Next_State;

      Print_Tape;
      Put_Line("Head Position: " & Integer'Image(Head_Position));
      Put_Line("Current State: " & Current_State);
      Put_Line("---");
   end Step;

   procedure Run_Turing_Machine is
   begin
      while Current_State /= "HALT" loop
         Step;
      end loop;
   end Run_Turing_Machine;

begin
   Tape_Buffer(1) := '1';
   Tape_Buffer(2) := '0';
   Tape_Buffer(3) := '1';

   Run_Turing_Machine;
end Turing_Machine;
