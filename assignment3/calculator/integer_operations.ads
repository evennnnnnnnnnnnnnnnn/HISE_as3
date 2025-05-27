

package Integer_Operations with SPARK_Mode is

   function Add(A: Integer; B: Integer) return Integer with
     Pre  => not ((B > 0 and then A > Integer'Last - B) or
                 (B < 0 and then A < Integer'First - B)),
     Post => Add'Result = A + B;

   function Subtract(A: Integer; B: Integer) return Integer with
     Pre  => not ((B < 0 and then A > Integer'Last + B) or
                 (B > 0 and then A < Integer'First + B)),
     Post => Subtract'Result = A - B;

   function Multiply (A: Integer; B: Integer) return Integer with
     Pre => not ((A > 0 and then B > 0 and then A > Integer'Last / B) or
                 (A < 0 and then B < 0 and then A < Integer'Last / B) or
                 (A < 0 and then B > 0 and then A < Integer'First / B) or
                 (A > 0 and then B < 0 and then B < Integer'First / A)),
     Post => Multiply'Result = A * B;


   function Divide(A: Integer; B: Integer) return Integer with
     Pre  => not (B = 0 or (A = Integer'First and then B = -1)),
     Post => Divide'Result = A / B;


end Integer_Operations;
