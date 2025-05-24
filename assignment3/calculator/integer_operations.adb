package body integer_operations with SPARK_Mode is

   
   function Add(X : Integer; Y : Integer) return Integer is
   begin
      return (X + Y);
   end Add;
   

   function Subtract(X : Integer; Y : Integer) return Integer is
   begin
      return (X - Y);
   end Subtract;
   
   function Multiply(X : Integer; Y : Integer) return Integer is
   begin
      return (X * Y);
   end Multiply;
   
   function Divide(X : Integer; Y : Integer) return Integer is
   begin
      return (X / Y);
   end Divide;

end integer_operations;
