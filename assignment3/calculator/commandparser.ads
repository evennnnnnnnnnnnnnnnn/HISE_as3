with MyStringTokeniser;       use MyStringTokeniser;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;

package commandParser is

   type Command_Kind is (Lock, Unlock, Push1, Push2, Pop, Add, Sub, Mul, Div, StoreTo, LoadFrom, Remove, List);
   
   type Command is private;
   
   function Parse_Command(Command_Line : String) return Command
     with Pre => Command_Line'Length > 0;
   
   function From_String(S : String) return Command_Kind
     with Pre => S'Length > 0;
   
   function Get_Cmd(Cmd : Command) return Command_Kind;
   
   function Get_Arg1(Cmd : Command) return Unbounded_String
     with Pre => (Get_Cmd(Cmd) in Push1 | Push2 | StoreTo | LoadFrom);
   
   function Get_Arg2(Cmd : Command) return Unbounded_String
     with Pre => (Get_Cmd(Cmd) in Push2);
      
private
   type Command is record
      Cmd : Command_Kind;
      Arg1 : Unbounded_String;
      Arg2 : Unbounded_String;
   end record;
   
end commandParser;
