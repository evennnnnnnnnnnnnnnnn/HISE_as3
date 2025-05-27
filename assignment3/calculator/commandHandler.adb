with Stack;                   use Stack;
with MemoryStore;
with StringToInteger;         use StringToInteger;
with Ada.Text_IO;             use Ada.Text_IO;
with MyStringTokeniser;       use MyStringTokeniser;
with PIN;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with MemoryStore;             use MemoryStore;
with Interfaces;
with integer_operations;      use integer_operations;

package body commandHandler with SPARK_Mode is

   procedure Handle_Lock(Cmd : in Command;
                         State : in out Boolean;
                         Master_Str : in out String;
                         Master_Pin : in out PIN.PIN) is
   begin
      -- UNLOCK <pin>
      if Get_Cmd(Cmd) = Unlock then
         if To_String(Get_Arg1(Cmd)) = Master_Str then
            State := True;
            Put_Line("Calculator unlocked.");
         else
            Put_Line("Error: wrong PIN");
         end if;

      -- LOCK <newpin>
      elsif Get_Cmd(Cmd) = Lock then
         declare
            New_Pin : constant String := To_String(Get_Arg1(Cmd));
         begin   
            if State and then Master_Str'Length = New_Pin'Length and then 
              (for all C of New_Pin => C in '0' .. '9') 
            then
               Master_Str := New_Pin;
               Master_Pin := PIN.From_String(Master_Str);
               State      := False;
               Put_Line("Calculator locked. New PIN set.");
               
            else
               Put_Line("Invalid PIN provided, must be 4 digit between 0000...9999");
            end if;
         end;
         
      end if;
   end Handle_Lock;
      
   procedure Handle_Stack(Cmd : in Command; State: in Boolean; S : in out Stack_type) is
   begin
      -- PUSH1 <num>
      if Get_Cmd(Cmd) = Push1 then
         declare
            V : constant Integer := From_String(To_String(Get_Arg1(Cmd)));
         begin
            -- Check invalid number provided, does not save
            if V = 0 and then To_String(Get_Arg1(Cmd))'Length /= 1 then
               Put_Line("Unable to 'Push1', invalid number provided");
               return;               
            end if;
            
            if Depth(S) < Max_Stack then
               Push(S, V);
            else
               Put_Line("Unable to 'Push1', stack is full");
            end if;
         end;

      -- PUSH2 <num1> <num2>
      elsif Get_Cmd(Cmd) = Push2 then
         declare
            V1 : constant Integer := From_String(To_String(Get_Arg1(Cmd)));
            V2 : constant Integer := From_String(To_String(Get_Arg2(Cmd)));
         begin
            -- Check invalid number provided, does not save
            if (V1 = 0 and then To_String(Get_Arg1(Cmd))'Length /= 1) or
               (V2 = 0 and then To_String(Get_Arg2(Cmd))'Length /= 1) then
               Put_Line("Unable to 'Push2', invalid number provided");
               return;               
            end if;
            if Depth(S) <= Max_Stack - 2 then
               Push2(S, V1, V2);
            else
               Put_Line("Unable to 'Push2', stack is full");
            end if;
         end;

      -- POP
      elsif Get_Cmd(Cmd) = Pop then
         if Depth(S) > 0 then
            Pop(S);
         else
            Put_Line("Unable to 'Pop', nothing on stack");
         end if;
      end if;
   end Handle_Stack;
   
   procedure Handle_Arithmetic(Cmd : in Command; State: in Boolean; S : in out Stack_type) is
   begin
      -- + (Add)
      if Get_Cmd(Cmd) = Add then
         if Depth(S) >= 2 then
            declare
               A : Integer := Second_Value(S);
               B : Integer := Top_Value(S);
            begin
               -- Check for potential overflow
               if (B > 0 and then A > Integer'Last - B) or
                 (B < 0 and then A < Integer'First - B)
               then
                  Put_Line("Unable to perform 'Addition' due to potential overflow"); 
               elsif Depth(S) < Max_Stack then
                  Pop(S);
                  Pop(S);
                  Push(S, Add(A, B));
               end if;
            end;
         else
            Put_Line("Unable to compute, insufficient operands");
         end if;

      -- - (Subtraction)
      elsif Get_Cmd(Cmd) = Sub then
         if Depth(S) >= 2 then
            declare
               A : Integer := Second_Value(S);
               B : Integer := Top_Value(S);
            begin
               -- Check for potential underflow
               if (B < 0 and then A > Integer'Last + B) or
                 (B > 0 and then A < Integer'First + B)
               then
                  Put_Line("Unable to perform 'Subtraction' due to potential underflow"); 
               elsif Depth(S) < Max_Stack then
                  Pop(S);
                  Pop(S);
                  Push(S, Subtract(A, B));
               end if;
            end;
         else
            Put_Line("Unable to compute, insufficient operands");
         end if;

      -- * (Multiplication)
      elsif Get_Cmd(Cmd) = Mul then
         if Depth(S) >= 2 then
            declare
               A : Integer := Second_Value(S);
               B : Integer := Top_Value(S);
            begin

               if (A > 0 and then B > 0 and then A > Integer'Last / B) or
                 (A < 0 and then B < 0 and then A < Integer'Last / B) or
                 (A < 0 and then B > 0 and then A < Integer'First / B) or
                 (A > 0 and then B < 0 and then B < Integer'First / A)
               then
                  Put_Line("Unable to perform 'Multiplication' due to potential overflow"); 
               elsif Depth(S) < Max_Stack then
                  Pop(S);
                  Pop(S);
                  Push(S, Multiply(A, B));
               end if;
            end;
         else
            Put_Line("Unable to compute, insufficient operands");
         end if;

      -- / (Division)
      elsif Get_Cmd(Cmd) = Div then
         if Depth(S) >= 2 then
            declare
               A : Integer := Second_Value(S);
               B : Integer := Top_Value(S);
            begin
               if B = 0 then
                  Put_Line("Unable to perform 'Division' due to division by 0");
               elsif A = Integer'First and then B = -1 then
                  Put_Line("Unable to perform 'Division' due to potential overflow");
               elsif Depth(S) < Max_Stack then
                  Pop(S);
                  Pop(S);
                  Push(S, Divide(A, B));
               end if;
            end;
         else
            Put_Line("Unable to compute, insufficient operands");
         end if;
      end if;
   end Handle_Arithmetic;
      
   procedure Handle_Memory(Cmd : in Command; State: in Boolean; S : in out Stack_type; Mem : in out MemoryStore.Database) is
   begin
      -- storeTo <loc>
      if Get_Cmd(Cmd) = StoreTo then
         if Depth(S) > 0 then
            declare
               Loc_Int : Integer := From_String(To_String(Get_Arg1(Cmd)));
            begin
               if Loc_Int in MemoryStore.Location_Index'Range then
                  MemoryStore.Put(Mem,
                    MemoryStore.Location_Index(Loc_Int),
                    Interfaces.Integer_32(Top_Value(S)));
                  Pop(S);
               end if;
            end;
         else
            Put_Line("Unable to store to memory, insufficient operands");
         end if;

      -- loadFrom <loc>
      elsif Get_Cmd(Cmd) = LoadFrom then
         declare
            Loc_Int : Integer := From_String(To_String(Get_Arg1(Cmd)));
         begin
            if Loc_Int in MemoryStore.Location_Index'Range and then
              MemoryStore.Has(Mem, MemoryStore.Location_Index(Loc_Int)) and then
              Depth(S) < Max_Stack
            then
               Push(S, Integer(MemoryStore.Get(Mem,
                    MemoryStore.Location_Index(Loc_Int))));
            else
               Put_Line("Invalid save location, between 1 and" & Integer'Image(Max_Locations) 
                       & ", or no number saved at that location. Try 'list' command to see.");
            end if;
         end;

      -- remove <loc>
      elsif Get_Cmd(Cmd) = Remove then
         declare
            Loc_Int : Integer := From_String(To_String(Get_Arg1(Cmd)));
         begin
            -- Check arg 1 validity, has to be between 
            if Loc_Int in MemoryStore.Location_Index'Range then
               MemoryStore.Remove(Mem,
                                  MemoryStore.Location_Index(Loc_Int));
            else
               Put_Line("Please provide valid save location, between 1 and" & Integer'Image(Max_Locations));
            end if;
         end;

      -- list
      elsif Get_Cmd(Cmd) = List then
         MemoryStore.Print(Mem);

      -- Unknown command
      else
         Put_Line("Error: unknown command");
      end if;
   end Handle_Memory;


end commandHandler;
