with commandParser; Use commandParser;
with MemoryStore;
with PIN; use PIN;
with Stack; use Stack;
with MyStringTokeniser;       use MyStringTokeniser;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with StringToInteger;

package commandHandler with SPARK_Mode is

   procedure Handle_Lock(Cmd : in Command;
                         State : in out Boolean;
                         Master_Str : in out String;
                         Master_Pin : in out PIN.PIN) with
     Pre => (if Get_Cmd(Cmd) = Unlock then not State) and (if Get_Cmd(Cmd) = Lock then State)
            and Master_Str'Length = 4;
     --  Post =>
     --   -- If UNLOCK and correct PIN, then State becomes True
     --     (if Get_Cmd(Cmd) = Unlock and then To_String(Get_Arg1(Cmd)) = Master_Str then State = True)
     --     and
     --   -- If LOCK and State was True, then State becomes False and Master_Pin is updated
     --  (if Get_Cmd(Cmd) = Lock and then State'Old = True and then
     --      Master_Str'Length = To_String(Get_Arg1(Cmd))'Length and then
     --       (for all C of To_String(Get_Arg1(Cmd)) => C in '0' .. '9')
     --           then
     --             Master_Str = To_String(Get_Arg1(Cmd)) and then
     --          State = False
     --        -- Check Master Pin updated when Lock is performed
     --        and then Master_Pin = From_String(Master_Str));


   procedure Handle_Stack(Cmd : in Command; State: in Boolean; S : in out Stack_type) with
     Pre => Get_Cmd(Cmd) in Push1 | Push2 | Pop and State;
     --
     --  -- If PUSH1 and PUSH2 can be successfully performed, element added on stack
     --  Post => (if Get_Cmd(Cmd) = Push1 and Depth(S) < Max_Stack then Depth(S) = Depth(S'Old) + 1
     --             else (if Get_Cmd(Cmd) = Push2 and Depth(S) <= Max_Stack - 2 then Depth(S) = Depth(S'Old) + 2
     --                   else (if Get_Cmd(Cmd) = Pop and Depth(S) > 0 then Depth(S) = Depth(S'Old) - 1
     --                ))) and
     --          (if Get_Cmd(Cmd) not in Push1 | Push2 | Pop then S = S'Old);


   procedure Handle_Arithmetic(Cmd : in Command; State: in Boolean; S : in out Stack_type) with
     Pre => Get_Cmd(Cmd) in Add | Sub | Mul | Div and State;
     --
     --  -- Stack size decrease by 1 if computation performed successfully
     --  Post => (Depth(S) = Depth(S'Old) - 1 or Depth(S) = Depth(S'Old));

   procedure Handle_Memory(Cmd : in Command; State: in Boolean; S : in out Stack_type; Mem : in out MemoryStore.Database) with
     Pre => Get_Cmd(Cmd) in StoreTo | LoadFrom | Remove | List and State;


end commandHandler;
