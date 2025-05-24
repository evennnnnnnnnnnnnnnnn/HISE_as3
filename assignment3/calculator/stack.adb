with Ada.Text_IO;             use Ada.Text_IO;

package body Stack with SPARK_Mode is
   procedure Initialize(S: out Stack_type) is
   begin
      S.Top := 0;
      S.Data := (others => 0);
   end Initialize;
   
   procedure Push(S: in out Stack_type; V: Integer) is
   begin
      if Depth(S) >= Max_Stack then
         return;
      end if;
      S.Top := S.Top + 1;
      S.Data(S.Top) := V;
   end Push;
   
   procedure Push2(S: in out Stack_type; V1, V2: Integer) is
   begin
      if Depth(S) > Max_Stack - 2 then
         raise Constraint_Error with "Stack overflow";
      end if;
      Push(S, V1);
      Push(S, V2);
   end Push2;
   
   procedure Pop(S: in out Stack_type) is
   begin
      if Depth(S) = 0 then
         raise Constraint_Error with "Stack underflow";
      end if;
      S.Top := S.Top - 1;
   end Pop;
   
   function Top_Value(S: Stack_type) return Integer is
   begin
      if Depth(S) = 0 then
         raise Constraint_Error with "Stack is empty";
      end if;
      return S.Data(S.Top);
   end Top_Value;
   
   function Second_Value(S: Stack_type) return Integer is
   begin
      if Depth(S) <= 1 then
         raise Constraint_Error with "Stack has less than 2 elements";
      end if;
      return S.Data(S.Top - 1);
   end Second_Value;
   
   function Depth(S: Stack_type) return Natural is
   begin
      return S.Top;
   end Depth;
   
end Stack;
