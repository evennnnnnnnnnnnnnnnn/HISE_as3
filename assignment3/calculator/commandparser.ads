pragma SPARK_Mode (On);
with MyStringTokeniser;       use MyStringTokeniser;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Strings;             -- for Ada.Strings.Both
with Ada.Strings.Fixed;      use Ada.Strings.Fixed;
package commandParser with SPARK_Mode is
   type Command_Kind is (Lock, Unlock, Push1, Push2, Pop,
                         Add, Sub, Mul, Div,
                         StoreTo, LoadFrom, Remove, List, Unknown);
   type Command is private;
   
   --  Parse a command line string
   --  Returns a command with Lock as default if parsing fails
   function Parse_Command (Command_Line : String) return Command
     with
       Pre  => Command_Line'Length > 0,
       Post => Get_Cmd (Parse_Command'Result) in Command_Kind;
   
   function From_String (S : String) return Command_Kind
     with
       Pre => (Trim (S, Ada.Strings.Both) in
                  "+" | "-" | "*" | "\\" |
                  "lock" | "unlock" |
                  "push1" | "push2" | "pop" |
                  "storeTo" | "loadFrom" |
                  "remove" | "list"),
       Post => From_String'Result in Command_Kind;
   
   function Get_Cmd  (Cmd : Command) return Command_Kind
     with Global => null, Post => Get_Cmd'Result in Command_Kind;
   function Get_Arg1 (Cmd : Command) return Unbounded_String
     with Global => null, Pre => (Get_Cmd(Cmd) in Lock | Unlock | Push1 | Push2 | StoreTo | LoadFrom | Remove);

   function Get_Arg2 (Cmd : Command) return Unbounded_String
     with Global => null, Pre => (Get_Cmd(Cmd) in Push2);
private
   type Command is record
      Cmd  : Command_Kind;
      Arg1 : Unbounded_String;
      Arg2 : Unbounded_String;
   end record;
end commandParser;
