with Ada.Text_IO;        use Ada.Text_IO;
with MyCommandLine;
with MyStringTokeniser;  use MyStringTokeniser;
with PIN;

package body Calculator is

   -----------------------------------------------------------------------------
   --  Process_Command
   --  Parse a single input line into tokens and handle "unlock" / "lock".
   procedure Process_Command(
     Command_Line : String;
     State        : in out Boolean;
     Master_Str   : in out String;
     Master_Pin   : in out PIN.PIN
   ) is
      Tokens     : TokenArray(1..10);
      Num_Tokens : Natural;
      Cmd        : String(1..10) := (others => ' ');
      Arg        : String(1..4)  := (others => ' ');
   begin
      -- Tokenise
      Tokenise(Command_Line, Tokens, Num_Tokens);

      -- Extract command name
      if Num_Tokens >= 1 then
         declare
            E : constant TokenExtent := Tokens(1);
         begin
            Cmd(1..E.Length) := Command_Line(E.Start .. E.Start + E.Length - 1);
         end;
      end if;

      -- Extract argument if present
      if Num_Tokens >= 2 then
         declare
            E : constant TokenExtent := Tokens(2);
         begin
            Arg(1..E.Length) := Command_Line(E.Start .. E.Start + E.Length - 1);
         end;
      end if;

      -- Handle UNLOCK
      if Cmd(1..6) = "unlock" and then Num_Tokens = 2 then
         if Arg = Master_Str then
            State := True;
            Put_Line("Calculator unlocked.");
         else
            Put_Line("Error: wrong PIN");
         end if;

      -- Handle LOCK
      elsif Cmd(1..4) = "lock" and then Num_Tokens = 2 then
         if State then
            Master_Str := Arg;
            Master_Pin := PIN.From_String(Master_Str);
            State      := False;
            Put_Line("Calculator locked. New PIN set.");
         else
            Put_Line("Error: already locked");
         end if;

      -- Fallback
      else
         Put_Line("Error: unknown command");
      end if;
   end Process_Command;

   -----------------------------------------------------------------------------
   --  Run
   --  Main REPL: prompt, read line, check length, dispatch.
   procedure Run is
      Master_Str : String(1..4) := MyCommandLine.Argument(1);
      Master_Pin : PIN.PIN      := PIN.From_String(Master_Str);
      State      : Boolean      := False;
      Line_Buf   : String(1..2048);
      Len        : Natural;
   begin
      loop
         -- Prompt
         if State then
            Put("unlocked> ");
         else
            Put("locked>   ");
         end if;

         -- Read
         Get_Line(Line_Buf, Len);
         if Len = 0 then
            Put_Line("Exiting.");
            exit;
         end if;
         if Len > Line_Buf'Length then
            Put_Line("Error: input too long");
            exit;
         end if;

         -- Dispatch
         Process_Command(Line_Buf(1..Len), State, Master_Str, Master_Pin);
      end loop;
   end Run;

end Calculator;
