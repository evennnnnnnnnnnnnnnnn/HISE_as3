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
         Put_Line("Unable to perform 'Push' as not enough space left on space");
         return;
      end if;
      
      S.Top := S.Top + 1;
      S.Data(S.Top) := V;
   end Push;
   
   procedure Push2(S: in out Stack_type; V1, V2: Integer) is
   begin
      if S.Top > Max_Stack - 2 then
         Put_Line("Unable to perform 'Push2' as not enough space left on space");
         return;
      end if;
      
      -- Direct implementation to avoid nested precondition issues
      S.Top := S.Top + 1;
      S.Data(S.Top) := V1;
      S.Top := S.Top + 1;
      S.Data(S.Top) := V2;
   end Push2;
   
   procedure Pop(S: in out Stack_type) is
   begin
      if S.Top = 0 then
         Put_Line("Unable to perform 'Pop' due to no elements on stack");
         return;
      end if;
      
      S.Top := S.Top - 1;
   end Pop;
   
end Stack;
