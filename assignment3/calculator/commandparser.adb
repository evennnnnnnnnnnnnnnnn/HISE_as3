pragma SPARK_Mode (On);

with MyStringTokeniser;       use MyStringTokeniser;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with MyStringTokeniser;       use MyStringTokeniser;
with Ada.Strings.Equal_Case_Insensitive; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;

package body commandParser is
   function Parse_Command(Command_Line: String) return Command is
      Tokens     : TokenArray(1 .. 10);
      Num_Tokens : Natural;
      Cmd_String : String(1 .. 10)              := (others => ' ');
      Arg1       : Unbounded_String             := Null_Unbounded_String;
      Arg2       : Unbounded_String             := Null_Unbounded_String;
      Cmd        : Command;
   begin
      pragma Assert (Command_Line'Length > 0, "Command line cannot be empty");
      pragma Assert (Command_Line'Length <= Integer'Last / 2);
      
      -- 1. Tokenise the input string
      Tokenise(Command_Line, Tokens, Num_Tokens);
      pragma Assert (Num_Tokens <= 10, "Too many tokens");

      -- 2. Extract the command name into Cmd
      if Num_Tokens >= 1 then
         declare
            E : constant TokenExtent := Tokens(1);
         begin
            pragma Assert (E.Start + E.Length - 1 <= Command_Line'Last, "Token extent out of bounds");
            Cmd_String(1 .. E.Length) := Command_Line(E.Start .. E.Start + E.Length - 1);
         end;
      end if;

      -- 3. Extract first argument (if any) into Arg1
      if Num_Tokens >= 2 then
         declare
            E   : constant TokenExtent := Tokens(2);
            Sub : constant String       := Command_Line(E.Start .. E.Start + E.Length - 1);
         begin
            pragma Assert (E.Start + E.Length - 1 <= Command_Line'Last, "Token extent out of bounds");
            Arg1 := To_Unbounded_String(Sub);
         end;
      end if;

      -- 4. Extract second argument (if any) into Arg2
      if Num_Tokens >= 3 then
         declare
            E   : constant TokenExtent := Tokens(3);
            Sub : constant String       := Command_Line(E.Start .. E.Start + E.Length - 1);
         begin
            pragma Assert (E.Start + E.Length - 1 <= Command_Line'Last, "Token extent out of bounds");
            Arg2 := To_Unbounded_String(Sub);
         end;
      end if;
      
      -- 5. Form Command
      Cmd.Cmd := From_String(Cmd_String);
      Cmd.Arg1 := Arg1;
      Cmd.Arg2 := Arg2;

      return Cmd;
   end Parse_Command;
      
   function From_String(S : String) return Command_Kind is   
      Trimmed : constant String := Trim(S, Both);
   begin
      pragma Assert (S'Length > 0, "Input string cannot be empty");
      
      if Trimmed = "+" then
         return Add;
      elsif Trimmed = "-" then
         return Sub;
      elsif Trimmed = "*" then
         return Mul;
      elsif Trimmed = "\" then
         return Div;
      elsif Trimmed = "lock" then
         return Lock;
      elsif Trimmed = "unlock" then
         return Unlock;
      elsif Trimmed = "push1" then
         return Push1;
      elsif Trimmed = "push2" then
         return Push2;
      elsif Trimmed = "pop" then
         return Pop;
      elsif Trimmed = "storeTo" then
         return StoreTo;
      elsif Trimmed = "loadFrom" then
         return LoadFrom;
      elsif Trimmed = "remove" then
         return Remove;
      elsif Trimmed = "list" then
         return List;
      else
         raise Constraint_Error with "Unknown command string: " & S;
      end if;
   end From_String;

   function Get_Cmd(Cmd : Command) return Command_Kind is
   begin
      return Cmd.Cmd;
   end Get_Cmd;
   
   function Get_Arg1(Cmd : Command) return Unbounded_String is
   begin
      return Cmd.Arg1;
   end Get_Arg1;
   
   function Get_Arg2(Cmd : Command) return Unbounded_String is
   begin
      return Cmd.Arg2;
   end Get_Arg2;
   
end commandParser;
