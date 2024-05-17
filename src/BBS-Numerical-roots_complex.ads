with BBS.Numerical.complex_real;
generic
  type F is digits <>;
package BBS.Numerical.roots_complex is
   package cmplx is new BBS.Numerical.complex_real(f'Base);
   use type cmplx.complex;
   type errors is (none, bad_args, no_solution);
   type test_func is access function (x : cmplx.complex) return cmplx.complex;
   --
   --  The Mueller algorithm uses three points to model a quadratic curve and uses
   --  that to find a candidate root.    Unlike the bisection method, Mueller's
   --  method does not require a root to be located within the three points.
   --  This can potentially be used to find complex roots, however this
   --  implementation does not.
   --
   --  This method will fail if the function value at the three points is equal.
   --
   --  In this implementation, the user provides two points and the third is
   --  generated as the average of these two points.  This keeps the call the
   --  same as the bisection and secant functions.
   --
   --  Note that for this algorithm, the x0 and x2 values are not necessarily
   --  meaningful as upper and lower bounds for the root, except that they are
   --  both set equal to the return value if the root is exact.
   --
   function mueller(test : test_func; x0, x2 : in out cmplx.complex;
            limit : Positive; err : out errors) return cmplx.complex;
end BBS.Numerical.roots_complex;
