package Stack is
   Max_Stack : constant := 512;

   type Stack_type is limited private;

   procedure Initialize(S: out Stack_type);

   procedure Push(S: in out Stack_type; V: Integer);

   procedure Push2(S: in out Stack_type; V1, V2: Integer);

   procedure Pop(S: in out Stack_type);

   function Top_Value(S: Stack_type) return Integer;

   function Second_Value(S: Stack_type) return Integer;

   function Depth(S: Stack_type) return Natural;

private
   type Stack_Array is array(1 .. Max_Stack) of Integer;

   type Stack_type is record
      Data: Stack_Array := (others => 0);
      Top: Natural := 0;
   end record;
end Stack;
