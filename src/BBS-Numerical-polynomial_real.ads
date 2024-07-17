generic
  type F is digits <>;
package BBS.Numerical.polynomial_real is
   type poly is array (Natural range  <>) of f'Base
      with dynamic_predicate => poly'First = 0;
   --
   --  Basic arithmatic operations
   --
   function "+" (Left, Right : poly) return poly;
   function "-" (Left, Right : poly) return poly;
   function "-" (Right : poly) return poly;
   function "*" (Left, Right : poly) return poly;
   function "*" (Left : f; Right : poly) return poly
      with post => ("*"'Result'Last = Right'Last);
   function "*" (Left : poly; Right : f) return poly
      with post => ("*"'Result'Last = Left'Last);
   --
   --  Division based on poldiv() in "Numerical Recipes in C", second
   --  edition, 1992 by William H Press, Saul A. Tuekolsky, William T.
   --  Vetterlink, and Brian P. Flannery, section 5.3.
   --
   --  u/v => q, r
   --
   --  The quotient and remainder may need to be trimmed to remove leading
   --  zero coefficients.
   --
   procedure divide(u, v : poly; q : out poly; r : out poly)
      with pre => (q'Last >= (u'Last - v'Last)) and
                  (r'Last >= (v'Last - 1)) and
                  (u'Last >= v'Last);
   --
   --  Evaluate a polynomial at x.
   --
   function evaluate(p : poly; x : f'Base) return f'Base;
   --
   --  Trims a polynomial by removing leading zero coefficients.
   --
   function trim(p : poly) return poly
      with post => (trim'Result'Last <= p'Last);
   --
   --  Return the order of a polynomial
   --
   function order(p : poly) return Natural;
   --
   --  Utility print procedure for debugging
   --
   procedure print(p : poly; fore, aft, exp : Natural);
   --
   --  Basic calculus
   --
   function integrate(p : poly; c : f'Base) return poly
      with post => (integrate'Result'Last = (p'Last + 1));
   function derivative(p : poly) return poly
      with post => (((derivative'Result'Last = (p'Last - 1)) and (p'Last > 0)) or
                    ((derivative'Result'Last = 0) and (p'Last = 0)));
end BBS.Numerical.polynomial_real;
