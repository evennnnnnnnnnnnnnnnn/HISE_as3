pragma SPARK_Mode (On);

with MyStringTokeniser; use MyStringTokeniser;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings; -- Ada.Strings.Both

package body commandParser is

   ------------------------------------------------------------------
   --  Helper: compute end index of a token slice (safe from overflow)
   ------------------------------------------------------------------
   function Token_End (E : TokenExtent) return Integer is
   begin
      pragma Assert (E.Length > 0);
      pragma Assert (E.Start <= Integer'Last - (E.Length - 1));
      return E.Start + (E.Length - 1);
   end Token_End;

   ------------------------------------------------------------------
   --  Parse a command line into a Command record
   ------------------------------------------------------------------
   function Parse_Command (Command_Line : String) return Command is
      pragma Assert (Command_Line'Length > 0);

      Tokens     : TokenArray (1 .. 10) := (others => (Start => Command_Line'First, Length => 0));
      Num_Tokens : Natural; -- set by Tokenise
      Cmd_Buffer : String (1 .. 10) := (others => ' ');
      Arg1       : Unbounded_String := Null_Unbounded_String;
      Arg2       : Unbounded_String := Null_Unbounded_String;
      R          : Command;
   begin
      -- 1. split into tokens
      Tokenise (Command_Line, Tokens, Num_Tokens);
      pragma Assert (Num_Tokens <= 10);

      -- 2. first token -> command keyword
      if Num_Tokens >= 1 then
         declare
            E       : constant TokenExtent := Tokens (1);
            End_Pos : constant Integer := Token_End (E);
         begin
            --  prove slice lengths and bounds
            pragma Assert (End_Pos <= Command_Line'Last);
                        pragma Assume (E.Length <= Cmd_Buffer'Length);

            Cmd_Buffer (1 .. E.Length) := Command_Line (E.Start .. End_Pos);
         end;
      end if;

      -- 3. second token -> first argument
      if Num_Tokens >= 2 then
         declare
            E       : constant TokenExtent := Tokens (2);
            End_Pos : constant Integer := Token_End (E);
         begin
            pragma Assert (End_Pos <= Command_Line'Last);
            Arg1 := To_Unbounded_String (Command_Line (E.Start .. End_Pos));
         end;
      end if;

      -- 4. third token -> second argument
      if Num_Tokens >= 3 then
         declare
            E       : constant TokenExtent := Tokens (3);
            End_Pos : constant Integer := Token_End (E);
         begin
            pragma Assert (End_Pos <= Command_Line'Last);
            Arg2 := To_Unbounded_String (Command_Line (E.Start .. End_Pos));
         end;
      end if;

      -- 5. assemble result
      R.Cmd  := From_String (Cmd_Buffer);
      R.Arg1 := Arg1;
      R.Arg2 := Arg2;
      return R;
   end Parse_Command;

   ------------------------------------------------------------------
   --  Map text to enumeration value
   ------------------------------------------------------------------
   function From_String (S : String) return Command_Kind is
      T : constant String := Trim (S, Ada.Strings.Both);
   begin
      pragma Assert (S'Length > 0);
      if T = "+"          then return Add;
      elsif T = "-"        then return Sub;
      elsif T = "*"        then return Mul;
      elsif T = "\\"       then return Div;
      elsif T = "lock"     then return Lock;
      elsif T = "unlock"   then return Unlock;
      elsif T = "push1"    then return Push1;
      elsif T = "push2"    then return Push2;
      elsif T = "pop"      then return Pop;
      elsif T = "storeTo"  then return StoreTo;
      elsif T = "loadFrom" then return LoadFrom;
      elsif T = "remove"   then return Remove;
      elsif T = "list"     then return List;
      else

      raise Constraint_Error with "Unknown command string";
   end if;
   end From_String;

   ------------------------------------------------------------------
   --  Getters
   ------------------------------------------------------------------
   function Get_Cmd  (Cmd : Command) return Command_Kind     is (Cmd.Cmd);
   function Get_Arg1 (Cmd : Command) return Unbounded_String is (Cmd.Arg1);
   function Get_Arg2 (Cmd : Command) return Unbounded_String is (Cmd.Arg2);

end commandParser;
