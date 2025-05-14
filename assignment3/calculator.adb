with Ada.Text_IO;           use Ada.Text_IO;
with MyCommandLine;
with MyStringTokeniser;     use MyStringTokeniser;
with StringToInteger;       use StringToInteger;
with PIN;

package body Calculator is

   --------------------------------------------------------------------------
   --  Global stack definitions
   --
   --  Max_Stack: maximum number of elements on the operand stack.
   --  Stack:     array to hold integer operands.
   --  Top:       current stack height (0 = empty).
   --------------------------------------------------------------------------
   Max_Stack : constant := 512;
   type Stack_Type is array(1 .. Max_Stack) of Integer;
   Stack      : Stack_Type := (others => 0);
   Top        : Natural    := 0;

   --------------------------------------------------------------------------
   --  Process_Command
   --
   --  Parses a single command line into tokens and dispatches:
   --    unlock <pin>
   --    lock   <newpin>
   --    push1  <num>
   --    push2  <num1> <num2>
   --    pop
   --  Any other command yields an uknown error.
   --------------------------------------------------------------------------
   procedure Process_Command(
     Command_Line : String;
     State        : in out Boolean;
     Master_Str   : in out String;     -- current master PIN (length = 4)
     Master_Pin   : in out PIN.PIN     -- PIN object corresponding to Master_Str
   ) is
      Tokens     : TokenArray(1 .. 10);
      Num_Tokens : Natural;
      Cmd        : String(1 .. 10) := (others => ' ');
      Arg1       : String := "";
      Arg2       : String := "";
   begin
      -- 1. Tokenise the input string
      Tokenise(Command_Line, Tokens, Num_Tokens);

      -- 2. Extract the command name into Cmd
      if Num_Tokens >= 1 then
         declare
            E : constant TokenExtent := Tokens(1);
         begin
            Cmd(1 .. E.Length) :=
              Command_Line(E.Start .. E.Start + E.Length - 1);
         end;
      end if;

      -- 3. Extract first argument (if any)
      if Num_Tokens >= 2 then
         declare
            E : constant TokenExtent := Tokens(2);
         begin
            Arg1 := Command_Line(E.Start .. E.Start + E.Length - 1);
         end;
      end if;

      -- 4. Extract second argument (for push2)
      if Num_Tokens >= 3 then
         declare
            E : constant TokenExtent := Tokens(3);
         begin
            Arg2 := Command_Line(E.Start .. E.Start + E.Length - 1);
         end;
      end if;

      -- 5. Handle commands

      -- UNLOCK <pin>: if correct, set State := True
      if Cmd(1 .. 6) = "unlock" and then Num_Tokens = 2 then
         if Arg1 = Master_Str then
            State := True;
            Put_Line("Calculator unlocked.");
         else
            Put_Line("Error: wrong PIN");
         end if;

      -- LOCK <newpin>: if unlocked, update PIN and set State := False
      elsif Cmd(1 .. 4) = "lock" and then Num_Tokens = 2 then
         if State then
            Master_Str := Arg1;
            Master_Pin := PIN.From_String(Master_Str);
            State      := False;
            Put_Line("Calculator locked. New PIN set.");
         else
            Put_Line("Error: already locked");
         end if;

      -- PUSH1 <num>: push one integer onto the stack (overflow safe)
      elsif Cmd(1 .. 5) = "push1" and then Num_Tokens = 2 then
         declare
            V : constant Integer := From_String(Arg1);
         begin
            if Top < Max_Stack then
               Top := Top + 1;
               Stack(Top) := V;
            else
               Put_Line("Error: stack overflow");
            end if;
         end;

      -- PUSH2 <num1> <num2>: push two integers onto the stack
      elsif Cmd(1 .. 5) = "push2" and then Num_Tokens = 3 then
         declare
            V1 : constant Integer := From_String(Arg1);
            V2 : constant Integer := From_String(Arg2);
         begin
            if Top <= Max_Stack - 2 then
               Top := Top + 1; Stack(Top) := V1;
               Top := Top + 1; Stack(Top) := V2;
            else
               Put_Line("Error: stack overflow");
            end if;
         end;

      -- POP: remove top element (underflow safe)
      elsif Cmd(1 .. 3) = "pop" and then Num_Tokens = 1 then
         if Top > 0 then
            Top := Top - 1;
         else
            Put_Line("Error: stack underflow");
         end if;

      -- Unknown command
      else
         Put_Line("Error: unknown command");
      end if;
   end Process_Command;

   --------------------------------------------------------------------------
   --  Run
   --
   --  Main REPL: prompt user, read a line, check length, and dispatch.
   --------------------------------------------------------------------------
   procedure Run is
      Pin_Str  : String(1 .. 4) := MyCommandLine.Argument(1);
      Pin_Val  : PIN.PIN       := PIN.From_String(Pin_Str);
      State    : Boolean        := False;
      Line_Buf : String(1 .. 2048);
      Len      : Natural;
   begin
      loop
         -- Print prompt showing locked/unlocked state
         if State then
            Put("unlocked> ");
         else
            Put("locked>   ");
         end if;

         -- Read a line of input
         Get_Line(Line_Buf, Len);

         -- Exit on empty input
         if Len = 0 then
            Put_Line("Exiting.");
            exit;
         end if;

         -- Check maximum length
         if Len > Line_Buf'Length then
            Put_Line("Error: input too long");
            exit;
         end if;

         -- Dispatch the trimmed input to Process_Command
         Process_Command(Line_Buf(1 .. Len), State, Pin_Str, Pin_Val);
      end loop;
   end Run;

end Calculator;
