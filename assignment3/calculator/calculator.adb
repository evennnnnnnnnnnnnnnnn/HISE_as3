with Ada.Text_IO;             use Ada.Text_IO;
with MyCommandLine;
with MyStringTokeniser;       use MyStringTokeniser;
with StringToInteger;         use StringToInteger;
with PIN;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with MemoryStore;
with Interfaces;

-- Stack
with Stack; use Stack;
with commandHandler; use commandHandler;
with commandParser; use commandParser;

package body Calculator is

   --------------------------------------------------------------------------
   --  Process_Command
   --------------------------------------------------------------------------
   procedure Process_Command(
     Command_Line : String;
     State        : in out Boolean;
     Master_Str   : in out String;     -- PIN string, length = 4
     Master_Pin   : in out PIN.PIN;    -- PIN object corresponding to Master_Str
     S            : in out Stack.Stack_type;
     Mem          : in out MemoryStore.Database
     ) is
      Cmd : Command;
   begin
      -- Parse the command with the Command Parser
      Cmd := Parse_Command(Command_Line);

      -- Process the command in the Command Handler
      case Get_Cmd(Cmd) is
         when Unlock | Lock =>
            Handle_Lock(Cmd, State, Master_Str, Master_Pin);
         when Push1 | Push2 | Pop =>
            Handle_Stack(Cmd, S);
         when Add | Sub | Mul | Div =>
            Handle_Arithmetic(Cmd, S);
         when StoreTo | LoadFrom | Remove | List =>
            Handle_Memory(Cmd, S, Mem);
         when others =>
            Put_Line("Error: Invalid Command");
      end case;

   end Process_Command;

   --------------------------------------------------------------------------
   --  Run
   --------------------------------------------------------------------------
   procedure Run is
      Pin_Str  : String(1 .. 4) := "1234";
      Pin_Val  : PIN.PIN       := PIN.From_String(Pin_Str);
      State    : Boolean       := False;
      Line_Buf : String(1 .. 2048);
      Len      : Natural;
      S        : Stack.Stack_type;
      Mem      : MemoryStore.Database;
   begin


      MemoryStore.Init(Mem);
      Initialize(S);

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
         Process_Command(Line_Buf(1 .. Len), State, Pin_Str, Pin_Val, S, Mem);
      end loop;
   end Run;

end Calculator;
