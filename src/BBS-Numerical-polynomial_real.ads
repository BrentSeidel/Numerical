generic
  type F is digits <>;
package BBS.Numerical.polynomial_real is
   type poly is array (Natural range  <>) of f'Base;
   --
   --  Basic arithmatic operations
   --
   function "+" (Left, Right : poly) return poly
      with pre => (Left'First = 0) and (Right'First = 0);
   function "-" (Left, Right : poly) return poly
      with pre => (Left'First = 0) and (Right'First = 0);
   function "*" (Left, Right : poly) return poly
      with pre => (Left'First = 0) and (Right'First = 0);
   function "*" (Left : f; Right : poly) return poly
      with pre => (Right'First = 0),
           post => ("*"'Result'Last = Right'Last);
   function "*" (Left : poly; Right : f) return poly
      with pre => (Left'First = 0),
           post => ("*"'Result'Last = Left'Last);
   --
   function evaluate(Self : in poly; x : f'Base) return f'Base;
   --
end BBS.Numerical.polynomial_real;
