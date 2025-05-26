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
-- as demonstrated in commandparser.ads: "Pre => Command_Line'Length > 0" and "Pre => (Trim (S, Ada.Strings.Both) in "+" | "-" | "*" | "\\" | "lock" | "unlock" | ...)".
-- Postconditions guarantee proper command type resolution with "Post => Get_Cmd (Parse_Command'Result) in Command_Kind", while global contracts maintain 
-- parser isolation by ensuring functions operate independently of external state with "Global => null", enhancing security and reliability.

pragma SPARK_Mode (On);

with MyCommandLine;
with MyString;
with MyStringTokeniser;
with StringToInteger;
with PIN;
with MemoryStore;
with Calculator;
with Ada.Text_IO;use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Long_Long_Integer_Text_IO;


procedure Main is
   --  Helper instantiation for bounded lines
   package Lines is new MyString (Max_MyString_Length => 2048);
   S    : Lines.MyString;

   --  Memory database demo
   Mem  : MemoryStore.Database;
   Loc1 : MemoryStore.Location_Index := 10;
   --  PIN demo
   PIN1 : PIN.PIN := PIN.From_String ("1234");
   PIN2 : PIN.PIN := PIN.From_String ("1234");
begin
   ------------------------------------------------------------------
   --  Command-line echo
   ------------------------------------------------------------------
   Put_Line("==> Entering Calculator.Run");
   Calculator.Run;
   Put_Line("==> Exiting Calculator.Run");
   
   
   Put(MyCommandLine.Command_Name); Put_Line(" is running!");
   Put("I was invoked with "); Put(MyCommandLine.Argument_Count,0); Put_Line(" arguments.");
   for Arg in 1..MyCommandLine.Argument_Count loop
      Put("Argument "); Put(Arg,0); Put(": """);
      Put(MyCommandLine.Argument(Arg)); Put_Line("""");
   end loop;
   
   ------------------------------------------------------------------
   --  MemoryStore CRUD(Create, Read, Update, Delete) demo
   ------------------------------------------------------------------

   MemoryStore.Init (Mem);
   
   

   Put_Line ("Storing 50 at location 10 ...");
   MemoryStore.Put (Mem, Loc1, 50);

   Put ("Location 10 now holds: ");
   
   if MemoryStore.Has (Mem, Loc1) then
      pragma Assert (MemoryStore.Has (Mem, Loc1));
      Ada.Integer_Text_IO.Put (
        Integer (MemoryStore.Get (Mem, Loc1)),
        0
      );
   else
      Put_Line ("<no value at loc " & Loc1'Image & ">");
   end if;
   New_Line;

   
   

   Put_Line ("Listing defined locations:");
   MemoryStore.Print (Mem);

   Put_Line ("Removing location 10 ...");
   MemoryStore.Remove (Mem, Loc1);

   if MemoryStore.Has (Mem, Loc1) then
      Put_Line ("Location 10 is still defined! (unexpected)");
   else
      Put_Line ("Location 10 successfully removed.");
   end if;
   
   ------------------------------------------------------------------
   --  Tokeniser demo
   ------------------------------------------------------------------
   Put_Line("Reading a line of input. Enter some text (at most 3 tokens): ");
   Lines.Get_Line(S);

   Put_Line("Splitting the text into at most 3 tokens");
   declare
      T : MyStringTokeniser.TokenArray(1..3) := (others => (Start => 1, Length => 0));
      NumTokens : Natural;
   begin
      pragma Assert (Lines.To_String(S)'Length <= Integer'Last / 2);
      MyStringTokeniser.Tokenise(Lines.To_String(S),T,NumTokens);
      Put("You entered "); Put(NumTokens); Put_Line(" tokens.");
      if NumTokens > 3 then
         Put_Line("You entered too many tokens --- I said at most 3");
         NumTokens := 3;
      end if;
      
      for I in 1..NumTokens loop
         declare
            TokStr : String := Lines.To_String(Lines.Substring(S,T(I).Start,T(I).Start+T(I).Length-1));
	      begin
            Put("Token "); Put(I); Put(" is: """);
            Put(TokStr); Put_Line("""");
         end;
      end loop;

   end;
   
   ------------------------------------------------------------------
   --  PIN equality demo
   ------------------------------------------------------------------
   If PIN."="(PIN1,PIN2) then
      Put_Line("The two PINs are equal, as expected.");
   end if;
   
   ------------------------------------------------------------------
   --  32-bit overflow / parsing demo (unchanged)
   ------------------------------------------------------------------
   declare
      Smallest_Integer : Integer := StringToInteger.From_String("-2147483648");
      R : Long_Long_Integer := 
        Long_Long_Integer(Smallest_Integer) * Long_Long_Integer(Smallest_Integer);
   begin
      Put_Line("This is -(2 ** 32) (where ** is exponentiation) :");
      Put(Smallest_Integer); New_Line;
      
      if R < Long_Long_Integer(Integer'First) or
         R > Long_Long_Integer(Integer'Last) then
         Put_Line("Overflow would occur when trying to compute the square of this number");
      end if;
         
   end;
   Put_Line("2 ** 32 is too big to fit into an Integer...");
   Put_Line("Hence when trying to parse it from a string, it is treated as 0:");
   Put(StringToInteger.From_String("2147483648")); New_Line;
   
      
end Main;
