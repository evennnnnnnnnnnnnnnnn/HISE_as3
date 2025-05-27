package body integer_operations with SPARK_Mode is

   
   function Add(A : Integer; B : Integer) return Integer is
   begin
      return (A + B);
   end Add;
   

   function Subtract(A : Integer; B : Integer) return Integer is
   begin
      return (A - B);
   end Subtract;
   
   function Multiply(A : Integer; B : Integer) return Integer is
   begin
      return (A * B);
   end Multiply;
   
   function Divide(A : Integer; B : Integer) return Integer is
   begin
      return (A / B);
   end Divide;

end integer_operations;
