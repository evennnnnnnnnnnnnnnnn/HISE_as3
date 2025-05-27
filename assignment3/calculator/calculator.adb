with Ada.Text_IO;             use Ada.Text_IO;
with MyCommandLine;           use MyCommandLine;
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

package body Calculator with SPARK_Mode is

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

      -- Process the command in the Command Handler with safety checks
      case Get_Cmd(Cmd) is
         when Unlock =>
            if not State then
              Handle_Lock(Cmd, State, Master_Str, Master_Pin);
            else
               Put_Line("Can only 'unlock' when calculator is locked");
            end if;
         when Lock =>
            if State then
               Handle_Lock(Cmd, State, Master_Str, Master_Pin);
            else
               Put_Line("Can only 'lock' when calculator is unlocked");
            end if;
         when Push1 | Push2 | Pop =>
            if State then
               Handle_Stack(Cmd, State, S);
            else
               Put_Line("Cannot perform this operation without unlocking");
            end if;
         when Add | Sub | Mul | Div =>
            if State then
               Handle_Arithmetic(Cmd, State, S);
            else
               Put_Line("Cannot perform this operation without unlocking");
            end if;
         when StoreTo | LoadFrom | Remove | List =>
            if State then
               Handle_Memory(Cmd, State, S, Mem);
            else
               Put_Line("Cannot perform this operation without unlocking");
            end if;
         when Unknown =>
            Put_Line("Error: Invalid Command, Please Try Again");
      end case;

   end Process_Command;

   --------------------------------------------------------------------------
   --  Run
   --------------------------------------------------------------------------
   procedure Run is
      Pin_Str  : String(1..4) := (others => ' ');
      Pin_Val  : PIN.PIN;
      State    : Boolean       := False;
      Line_Buf : String(1 .. 2048);
      Len      : Natural;
      S        : Stack.Stack_type;
      Mem      : MemoryStore.Database;
   begin
      -- Process argument to set initial password
      if Argument_Count >= 1 then
         for I in MyCommandLine.Argument(1)'Range loop
            exit when I - MyCommandLine.Argument(1)'First + 1 > 4;
            Pin_Str(I - MyCommandLine.Argument(1)'First + 1) := MyCommandLine.Argument(1)(I);
         end loop;
         Put_Line("PIN is set to provided arg, up to 4 char");
      else
         Put_Line("No default password provided, password set to 1234");
         Pin_Str := "1234";
      end if;


      -- Initialize stack and memory
      MemoryStore.Init(Mem);
      Initialize(S);


      if Pin_Str'Length = 4 and then
        (for all C of Pin_Str => C in '0' .. '9') then
         Pin_Val := PIN.From_String(Pin_Str);
      else
         Put_Line("Invalid PIN format.");
         return;
      end if;

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
