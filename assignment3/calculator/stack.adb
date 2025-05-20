with Ada.Text_IO;             use Ada.Text_IO;

package body Stack is
   procedure Initialize(S: out Stack_type) is
   begin
      S.Top := 0;
      S.Data := (others => 0);
   end Initialize;
   
   procedure Push(S: in out Stack_type; V: Integer) is
   begin
      if S.Top < Max_Stack then
         S.Top := S.Top + 1;
         S.Data(S.Top) := V;
      else
         Put_Line("Error: Stack overflow");
      end if;
   end Push;
   
   procedure Push2(S: in out Stack_type; V1, V2: Integer) is
   begin
      if S.Top < Max_Stack - 1 then
         Push(S, V1);
         Push(S, V2);
      else
         Put_Line("Error: Stack overflow");
      end if;
   end Push2;
   
   procedure Pop(S: in out Stack_type) is
   begin
      if S.Top > 0 then
         S.Top := S.Top - 1;
      else
         Put_Line("Error: Stack underflow");
      end if;
   end Pop;
   
   function Top_Value(S: Stack_type) return Integer is
   begin
      return S.Data(S.Top);
   end Top_Value;
   
   function Second_Value(S: Stack_type) return Integer is
   begin
      if S.Top > 1 then
         return S.Data(S.Top - 1);
      else
         Put_Line("Error: Stack underflow");
         return 0;
      end if;
   end Second_Value;
   
   function Depth(S: Stack_type) return Natural is
   begin
      return S.Top;
   end Depth;


end Stack;
