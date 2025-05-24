package Stack with SPARK_Mode is
   Max_Stack : constant := 512;

   type Stack_type is private;
   
   type Stack_Array is array(1 .. Max_Stack) of Integer;

   procedure Initialize(S: out Stack_type);

   procedure Push(S: in out Stack_type; V: Integer)
     with Pre => Depth(S) < Max_Stack,
          Post => Depth(S) = Depth(S'Old) + 1 and Top_Value(S) = V;

   procedure Push2(S: in out Stack_type; V1, V2: Integer)
     with Pre => Depth(S) <= Max_Stack - 2,
          Post => Depth(S) = Depth(S)'Old + 2 and 
                  Top_Value(S) = V2 and 
                  Second_Value(S) = V1;

   procedure Pop(S: in out Stack_type)
     with Pre => Depth(S) > 0,
          Post => Depth(S) = Depth(S'Old) - 1;

   function Top_Value(S: Stack_type) return Integer
     with Pre => Depth(S) > 0;

   function Second_Value(S: Stack_type) return Integer
     with Pre => Depth(S) > 1;

   function Depth(S: Stack_type) return Natural with Global => null;
   
private
   type Stack_type is record
      Data: Stack_Array := (others => 0);
      Top: Natural := 0;
   end record;
end Stack;
