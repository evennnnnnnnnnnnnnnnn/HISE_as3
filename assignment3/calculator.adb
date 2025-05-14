with Ada.Text_IO;             use Ada.Text_IO;
with MyCommandLine;
with MyStringTokeniser;       use MyStringTokeniser;
with StringToInteger;         use StringToInteger;
with PIN;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with MemoryStore;
with Interfaces;
package body Calculator is

   --------------------------------------------------------------------------
   --  Global stack definitions
   --------------------------------------------------------------------------
   Max_Stack : constant := 512;
   type Stack_Type is array(1 .. Max_Stack) of Integer;
   Stack      : Stack_Type := (others => 0);
   Top        : Natural    := 0;
   Mem : MemoryStore.Database;
   --------------------------------------------------------------------------
   --  Process_Command
   --------------------------------------------------------------------------
   procedure Process_Command(
     Command_Line : String;
     State        : in out Boolean;
     Master_Str   : in out String;     -- PIN string, length = 4
     Master_Pin   : in out PIN.PIN     -- PIN object corresponding to Master_Str
   ) is
      Tokens     : TokenArray(1 .. 10);
      Num_Tokens : Natural;
      Cmd        : String(1 .. 10)              := (others => ' ');
      Arg1       : Unbounded_String             := Null_Unbounded_String;
      Arg2       : Unbounded_String             := Null_Unbounded_String;
   begin
      -- 1. Tokenise the input string
      Tokenise(Command_Line, Tokens, Num_Tokens);

      -- 2. Extract the command name into Cmd
      if Num_Tokens >= 1 then
         declare
            E : constant TokenExtent := Tokens(1);
         begin
            Cmd(1 .. E.Length) := Command_Line(E.Start .. E.Start + E.Length - 1);
         end;
      end if;

      -- 3. Extract first argument (if any) into Arg1
      if Num_Tokens >= 2 then
         declare
            E   : constant TokenExtent := Tokens(2);
            Sub : constant String       := Command_Line(E.Start .. E.Start + E.Length - 1);
         begin
            Arg1 := To_Unbounded_String(Sub);
         end;
      end if;

      -- 4. Extract second argument (if any) into Arg2
      if Num_Tokens >= 3 then
         declare
            E   : constant TokenExtent := Tokens(3);
            Sub : constant String       := Command_Line(E.Start .. E.Start + E.Length - 1);
         begin
            Arg2 := To_Unbounded_String(Sub);
         end;
      end if;

      -- 5. Handle commands

      -- UNLOCK <pin>
      if Cmd(1 .. 6) = "unlock" and then Num_Tokens = 2 then
         if To_String(Arg1) = Master_Str then
            State := True;
            Put_Line("Calculator unlocked.");
         else
            Put_Line("Error: wrong PIN");
         end if;

      -- LOCK <newpin>
      elsif Cmd(1 .. 4) = "lock" and then Num_Tokens = 2 then
         if State then
            Master_Str := To_String(Arg1);
            Master_Pin := PIN.From_String(Master_Str);
            State      := False;
            Put_Line("Calculator locked. New PIN set.");
         else
            Put_Line("Error: already locked");
         end if;

      -- PUSH1 <num>
      elsif Cmd(1 .. 5) = "push1" and then Num_Tokens = 2 then
         declare
            V : constant Integer := From_String(To_String(Arg1));
         begin
            if Top < Max_Stack then
               Top := Top + 1;
               Stack(Top) := V;
            else
               Put_Line("Error: stack overflow");
            end if;
         end;

      -- PUSH2 <num1> <num2>
      elsif Cmd(1 .. 5) = "push2" and then Num_Tokens = 3 then
         declare
            V1 : constant Integer := From_String(To_String(Arg1));
            V2 : constant Integer := From_String(To_String(Arg2));
         begin
            if Top <= Max_Stack - 2 then
               Top := Top + 1; Stack(Top) := V1;
               Top := Top + 1; Stack(Top) := V2;
            else
               Put_Line("Error: stack overflow");
            end if;
         end;

      -- POP
      elsif Cmd(1 .. 3) = "pop" and then Num_Tokens = 1 then
         if Top > 0 then
            Top := Top - 1;
         else
            Put_Line("Error: stack underflow");
         end if;
      elsif Cmd(1 .. 1) = "+" and then Num_Tokens = 1 then
         if not State then
            Put_Line("Error: calculator locked");
         elsif Top >= 2 then
            declare
               Op1 : constant Integer := Stack(Top - 1);
               Op2 : constant Integer := Stack(Top);
               Result : Integer;
            begin
               -- Check for potential overflow
               if (Op1 > 0 and then Op2 > 0 and then Op1 > Integer'Last - Op2) or
                 (Op1 < 0 and then Op2 < 0 and then Op1 < Integer'First - Op2) then
                  Put_Line("Error: addition would cause overflow");
               else
                  Result := Op1 + Op2;
                  Top := Top - 2;
                  Top := Top + 1;
                  Stack(Top) := Result;
               end if;
            end;
         else
            Put_Line("Error: insufficient operands");
         end if;

         -- - (Subtraction)
      elsif Cmd(1 .. 1) = "-" and then Num_Tokens = 1 then
         if not State then
            Put_Line("Error: calculator locked");
         elsif Top >= 2 then
            declare
               Op1 : constant Integer := Stack(Top - 1);
               Op2 : constant Integer := Stack(Top);
               Result : Integer;
            begin
               -- Check for potential overflow
               if (Op1 > 0 and then Op2 < 0 and then Op1 > Integer'Last + Op2) or
                 (Op1 < 0 and then Op2 > 0 and then Op1 < Integer'First + Op2) then
                  Put_Line("Error: subtraction would cause overflow");
               else
                  Result := Op1 - Op2;
                  Top := Top - 2;
                  Top := Top + 1;
                  Stack(Top) := Result;
               end if;
            end;
         else
            Put_Line("Error: insufficient operands");
         end if;



            -- storeTo <loc>
      elsif Cmd(1 .. 7) = "storeTo" and then Num_Tokens = 2 then
         if not State then
            Put_Line("Error: calculator locked");
         elsif Top > 0 then
            declare
               Loc_Int : Integer := From_String(To_String(Arg1));
            begin
               if Loc_Int in MemoryStore.Location_Index'Range then
                  MemoryStore.Put(Mem,
                    MemoryStore.Location_Index(Loc_Int),
                    Interfaces.Integer_32(Stack(Top)));
                  Top := Top - 1;
               end if;
            end;
         else
            Put_Line("Error: insufficient operands");
         end if;

      -- loadFrom <loc>
      elsif Cmd(1 .. 8) = "loadFrom" and then Num_Tokens = 2 then
         if not State then
            Put_Line("Error: calculator locked");
         else
            declare
               Loc_Int : Integer := From_String(To_String(Arg1));
            begin
               if Loc_Int in MemoryStore.Location_Index'Range and then
                  MemoryStore.Has(Mem, MemoryStore.Location_Index(Loc_Int))
               then
                  Top := Top + 1;
                  Stack(Top) :=
                    Integer(MemoryStore.Get(Mem,
                      MemoryStore.Location_Index(Loc_Int)));
               end if;
            end;
         end if;

      -- remove <loc>
      elsif Cmd(1 .. 6) = "remove" and then Num_Tokens = 2 then
         if not State then
            Put_Line("Error: calculator locked");
         else
            declare
               Loc_Int : Integer := From_String(To_String(Arg1));
            begin
               if Loc_Int in MemoryStore.Location_Index'Range then
                  MemoryStore.Remove(Mem,
                    MemoryStore.Location_Index(Loc_Int));
               end if;
            end;
         end if;

      -- list
      elsif Cmd(1 .. 4) = "list" and then Num_Tokens = 1 then
         if not State then
            Put_Line("Error: calculator locked");
         else
            MemoryStore.Print(Mem);
         end if;

      -- Unknown command
      else
         Put_Line("Error: unknown command");
      end if;

   end Process_Command;

   --------------------------------------------------------------------------
   --  Run
   --------------------------------------------------------------------------
   procedure Run is
      Pin_Str  : String(1 .. 4) := MyCommandLine.Argument(1);
      Pin_Val  : PIN.PIN       := PIN.From_String(Pin_Str);
      State    : Boolean       := False;
      Line_Buf : String(1 .. 2048);
      Len      : Natural;
   begin
      MemoryStore.Init(Mem);
      loop
         -- Prompt
         if State then
            Put("unlocked> ");
         else
            Put("locked>   ");
         end if;

         -- Read input
         Get_Line(Line_Buf, Len);

         -- Exit on empty input
         if Len = 0 then
            Put_Line("Exiting.");
            exit;
         end if;

         -- Length check
         if Len > Line_Buf'Length then
            Put_Line("Error: input too long");
            exit;
         end if;

         -- Dispatch
         Process_Command(Line_Buf(1 .. Len), State, Pin_Str, Pin_Val);
      end loop;
   end Run;

end Calculator;
