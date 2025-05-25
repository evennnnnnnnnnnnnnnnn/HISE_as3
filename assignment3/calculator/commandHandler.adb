with Stack; use Stack;
with MemoryStore;
with StringToInteger;         use StringToInteger;
with Ada.Text_IO;             use Ada.Text_IO;
with MyStringTokeniser;       use MyStringTokeniser;
with PIN;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with MemoryStore;
with Interfaces;

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
            New_Pin : String := To_String(Get_Arg1(Cmd));
         begin   
            if State and then Master_Str'Length = New_Pin'Length and then 
              (for all C of New_Pin => C in '0' .. '9') 
            then
               Master_Str := To_String(Get_Arg1(Cmd));
               Master_Pin := PIN.From_String(Master_Str);
               State      := False;
               Put_Line("Calculator locked. New PIN set.");
            else
               Put_Line("Error: already locked");
            end if;
         end;
         
      end if;
   end Handle_Lock;
      
   procedure Handle_Stack(Cmd : in Command; S : in out Stack_type) is
   begin
      -- PUSH1 <num>
      if Get_Cmd(Cmd) = Push1 then
         declare
            V : constant Integer := From_String(To_String(Get_Arg1(Cmd)));
         begin
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
   
   procedure Handle_Arithmetic(Cmd : in Command; S : in out Stack_type) is
   begin
      -- + (Add)
      if Get_Cmd(Cmd) = Add then
         if Depth(S) >= 2 then
            declare
               A : Integer := Second_Value(S);
               B : Integer := Top_Value(S);
            begin
               -- Check for potential overflow
               if (A > 0 and then B > 0 and then A <= Integer'Last - B) and
                 (A < 0 and then B < 0 and then A >= Integer'First - B)
               then
                  if Depth(S) < Max_Stack then
                     Pop(S);
                     Pop(S);
                     Push(S, A + B);
                  end if;
               else
                  Put_Line("Unable to perform 'Addition' due to potential overflow"); 
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
               if (A > 0 and then B < 0 and then A <= Integer'Last + B) and
                 (A < 0 and then B > 0 and then A >= Integer'First + B)
               then
                  if Depth(S) < Max_Stack then
                     Pop(S);
                     Pop(S);
                     Push(S, A - B);
                  end if;
               else
                  Put_Line("Unable to perform 'Subtraction' due to potential underflow"); 
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
               else
                  if Depth(S) < Max_Stack then
                     Pop(S);
                     Pop(S);
                     Push(S, A * B);
                  end if;
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
                  Push(S, A / B);
               end if;
            end;
         else
            Put_Line("Unable to compute, insufficient operands");
         end if;
      end if;
   end Handle_Arithmetic;
      
   procedure Handle_Memory(Cmd : in Command; S : in out Stack_type; Mem : in out MemoryStore.Database) is
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
            end if;
         end;

      -- remove <loc>
      elsif Get_Cmd(Cmd) = Remove then
         declare
            Loc_Int : Integer := From_String(To_String(Get_Arg1(Cmd)));
         begin
            if Loc_Int in MemoryStore.Location_Index'Range then
               MemoryStore.Remove(Mem,
                 MemoryStore.Location_Index(Loc_Int));
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
