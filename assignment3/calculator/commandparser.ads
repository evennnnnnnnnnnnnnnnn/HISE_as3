with MyStringTokeniser;       use MyStringTokeniser;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Big_Integers; use Big_Integers;

package commandParser with SPARK_Mode is

   type Command_Kind is (Lock, Unlock, Push1, Push2, Pop, Add, Sub, Mul, Div, StoreTo, LoadFrom, Remove, List, Unknown);
   
   type Command is private;
   
   function Parse_Command(Command_Line : String) return Command;
   
   function Safe_Slice(Command : String; Ext : TokenExtent) return String
     with
       Pre =>
         Ext.Start in Command'Range and then
         Ext.Length >= 1 and then
         (In_Range(Arg => To_Big_Integer(Ext.Start) + To_Big_Integer(Ext.Length - 1),
                   Low => To_Big_Integer(Command'First),
                   High => To_Big_Integer(Command'Last))),
      Post =>
        Safe_Slice'Result'Length = Ext.Length;
   
   function From_String(S : String) return Command_Kind
     with Pre => S'Length > 0;
   
   function Get_Cmd(Cmd : Command) return Command_Kind with Global => null;
   
   function Get_Arg1(Cmd : Command) return Unbounded_String
     with Global => null, Pre => (Get_Cmd(Cmd) in Lock | Unlock | Push1 | Push2 | StoreTo | LoadFrom | Remove);
   
   function Get_Arg2(Cmd : Command) return Unbounded_String
     with Global => null, Pre => (Get_Cmd(Cmd) in Push2);
      
private
   type Command is record
      Cmd : Command_Kind;
      Arg1 : Unbounded_String;
      Arg2 : Unbounded_String;
   end record;
   
end commandParser;
