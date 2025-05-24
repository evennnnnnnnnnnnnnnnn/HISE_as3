with Big_Integers; use Big_Integers;

package Integer_Operations with SPARK_Mode is

   --  function Add(X : Integer; Y : Integer) return Integer with
   --    Pre => (In_Range(Arg => To_Big_Integer(X) + To_Big_Integer(Y),
   --                     Low => To_Big_Integer(Integer'First),
   --                     High => To_Big_Integer(Integer'Last))),
   --    Post => Add'Result = To_Integer(To_Big_Integer(X) + To_Big_Integer(Y));

   function Add(X: Integer; Y: Integer) return Integer with
     Pre  => (X > 0 and then Y > 0 and then X <= Integer'Last - Y) or
     (X < 0 and then Y < 0 and then X >= Integer'First - Y) or
     (X >= 0 and then Y < 0) or
     (X < 0 and then Y >= 0),
     Post => Add'Result = X + Y;

   function Subtract(X: Integer; Y: Integer) return Integer with
     Pre  => (X > 0 and then Y < 0 and then X <= Integer'Last + Y) or
     (X < 0 and then Y > 0 and then X >= Integer'First + Y) or
     (X >= 0 and then Y >= 0) or
     (X < 0 and then Y < 0),
     Post => Subtract'Result = X - Y;

   function Multiply (X: Integer; Y: Integer) return Integer with
     Pre => (X = 0 or Y = 0) and
     (X > 0 and then Y > 0 and then X <= Integer'Last / Y) and
     (X < 0 and then Y < 0 and then Y <= Integer'Last / X) and
     (X < 0 and then Y > 0 and then X >= Integer'First / Y) and
     (X > 0 and then Y < 0 and then Y >= Integer'First / X),
     Post => Multiply'Result = X * Y;


   function Divide(X: Integer; Y: Integer) return Integer with
     Pre  => Y /= 0 and then
     (X /= Integer'First or Y /= -1),
     Post => Divide'Result = X / Y;

end Integer_Operations;
