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
   function evaluate(p : poly; x : f'Base) return f'Base;
   --
   --  Basic calculus
   --
   function integrate(p : poly; c : f'Base) return poly
      with post => (integrate'Result'Last = (p'Last + 1));
   function derivative(p : poly) return poly
      with post => (((derivative'Result'Last = (p'Last - 1)) and (p'Last > 0)) or
                    ((derivative'Result'Last = 0) and (p'Last = 0)));
end BBS.Numerical.polynomial_real;
