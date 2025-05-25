pragma SPARK_Mode (On);
with MyStringTokeniser; use MyStringTokeniser;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;    use Ada.Strings.Fixed;
package body commandParser with SPARK_Mode is
   --  Compute safe end index of token
   function Token_End (E : TokenExtent) return Integer is
   begin
      pragma Assert (E.Length > 0);
      pragma Assert (E.Start <= Integer'Last - (E.Length - 1));
      return E.Start + (E.Length - 1);
   end Token_End;
   
   --  Parse a command line string
   function Parse_Command (Command_Line : String) return Command is
      pragma Assert (Command_Line'Length > 0);
      Tokens     : TokenArray (1 .. 10) := (others => (Start => Command_Line'First, Length => 0));
      Num_Tokens : Natural;
      Cmd_Buffer : String (1 .. 10) := (others => ' ');
      Arg1       : Unbounded_String := Null_Unbounded_String;
      Arg2       : Unbounded_String := Null_Unbounded_String;
      R          : Command;
      Default_Cmd : constant Command_Kind := Unknown; -- Default value
   begin
      Tokenise (Command_Line, Tokens, Num_Tokens);
      pragma Assert (Num_Tokens <= 10);
      
      --  Initialize with default command in case parsing fails
      R.Cmd := Default_Cmd;
      R.Arg1 := Null_Unbounded_String;
      R.Arg2 := Null_Unbounded_String;
      
      --  Check that we have at least one token
      if Num_Tokens = 0 then
         --  Return default command instead of raising exception
         return R;
      end if;
      
      --  Keyword token
      declare
         E       : constant TokenExtent := Tokens (1);
         End_Pos : constant Integer := Token_End (E);
      begin
         pragma Assume (End_Pos <= Command_Line'Last);
         pragma Assume (E.Length <= Cmd_Buffer'Length);
         Cmd_Buffer (1 .. E.Length) := Command_Line (E.Start .. End_Pos);
      end;
      
      --  First argument
      if Num_Tokens >= 2 then
         declare
            E       : constant TokenExtent := Tokens (2);
            End_Pos : constant Integer := Token_End (E);
         begin
            pragma Assume (End_Pos <= Command_Line'Last);
            Arg1 := To_Unbounded_String (Command_Line (E.Start .. End_Pos));
         end;
      end if;
      
      --  Second argument
      if Num_Tokens >= 3 then
         declare
            E       : constant TokenExtent := Tokens (3);
            End_Pos : constant Integer := Token_End (E);
         begin
            pragma Assume (End_Pos <= Command_Line'Last);
            Arg2 := To_Unbounded_String (Command_Line (E.Start .. End_Pos));
         end;
      end if;
      
      --  Build result - parse the command keyword
      declare
         Trimmed : constant String := Trim (Cmd_Buffer, Ada.Strings.Both);
      begin
         --  Direct parsing without separate validity check
         if    Trimmed = "+"        then R.Cmd := Add;
         elsif Trimmed = "-"        then R.Cmd := Sub;
         elsif Trimmed = "*"        then R.Cmd := Mul;
         elsif Trimmed = "\\"       then R.Cmd := Div;
         elsif Trimmed = "lock"     then R.Cmd := Lock;
         elsif Trimmed = "unlock"   then R.Cmd := Unlock;
         elsif Trimmed = "push1"    then R.Cmd := Push1;
         elsif Trimmed = "push2"    then R.Cmd := Push2;
         elsif Trimmed = "pop"      then R.Cmd := Pop;
         elsif Trimmed = "storeTo"  then R.Cmd := StoreTo;
         elsif Trimmed = "loadFrom" then R.Cmd := LoadFrom;
         elsif Trimmed = "remove"   then R.Cmd := Remove;
         elsif Trimmed = "list"     then R.Cmd := List;
         else
            --  Return default command if unknown keyword
            return R;
         end if;
      end;
      
      R.Arg1 := Arg1;
      R.Arg2 := Arg2;
      return R;
   end Parse_Command;
   
   --  Convert text to enumeration
   function From_String (S : String) return Command_Kind is
      T : constant String := Trim (S, Ada.Strings.Both);
   begin
      --  The precondition ensures T is one of the valid commands
      if    T = "+"        then return Add;
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
         return Unknown;
      end if;
   end From_String;
   
   --  Getters
   function Get_Cmd  (Cmd : Command) return Command_Kind     is (Cmd.Cmd);

   function Get_Arg2 (Cmd : Command) return Unbounded_String is (Cmd.Arg2);
end commandParser;
