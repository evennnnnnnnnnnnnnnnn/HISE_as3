-- Task4
--
-- Lock Operation & PIN Update Security Property:
-- SPARK annotations verify operation state and PIN format through preconditions, ensuring lock operations are executed in the correct state.
-- For example, in commandHandler.ads: "Pre => (if Get_Cmd(Cmd) = Unlock then not State) and (if Get_Cmd(Cmd) = Lock then State)".
-- Postconditions guarantee correct Master_PIN updates while validating state transition integrity, as shown in "Post => (if Get_Cmd(Cmd) = Unlock and then 
-- To_String(Get_Arg1(Cmd)) = Master_Str then State = True)". Type system constraints ensure PIN values remain within valid range, preventing the use of 
-- illegal values.
--
-- Stack Operation Security Property:
-- SPARK annotations enforce stack safety through comprehensive preconditions and postconditions. Preconditions verify stack capacity and operation validity, 
-- ensuring operations like Push and Pop are executed within bounds, as shown in stack.ads: "Pre => Depth(S) < Max_Stack" for Push operations and 
-- "Pre => Depth(S) > 0" for Pop operations. Postconditions maintain data integrity by guaranteeing correct stack state updates after each operation, 
-- demonstrated by "Post => Depth(S) = Depth(S'Old) + 1 and Top_Value(S) = V" for Push. Type invariants prevent buffer overflow by constraining the stack 
-- pointer within valid range, implemented as "Type_Invariant => Top <= Max_Stack".
--
-- Command Parser Security Property:
-- SPARK annotations ensure robust command parsing through strict input validation. Preconditions verify command syntax and argument validity, 
-- as demonstrated in commandparser.ads: "Pre => Command_Line'Length > 0" and "Pre => (Trim (S, Ada.Strings.Both) in "+" | "-" | "*" | "\" | "lock" | "unlock" | ...)".
-- Postconditions guarantee proper command type resolution with "Post => Get_Cmd (Parse_Command'Result) in Command_Kind", while global contracts maintain 
-- parser isolation by ensuring functions operate independently of external state with "Global => null", enhancing security and reliability.
--
-- Arithmetic Computation Security Property:
-- SPARK annotations make sure arithmetic operations are strictly within 32-bit integer range through preconditions and postconditions. Preconditions
-- verify edges cases such as divide by zero in divisions, underflow and overflow in all operations. For instance, (B > 0 and then A > Integer'Last - B) or
-- (B < 0 and then A < Integer'First - B) helps detect whether the result would exceed the allowable range for a 32-bit integer in addition. Postconditions 
-- ensures valid operations are executed correctly. 


pragma SPARK_Mode (On);

with Calculator;
with Ada.Text_IO;use Ada.Text_IO;


procedure Main is
begin
   ------------------------------------------------------------------
   --  Start Calculator
   ------------------------------------------------------------------
   Put_Line("==> Entering Calculator.Run");
   Calculator.Run;
   Put_Line("==> Exiting Calculator.Run");
   

      
end Main;
