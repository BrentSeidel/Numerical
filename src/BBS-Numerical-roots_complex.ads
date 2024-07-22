with Ada.Numerics.Generic_Complex_Types;
generic
   with package cmplx is new Ada.Numerics.Generic_Complex_Types(<>);
   use type cmplx.Complex;
   use type cmplx.Real;
package BBS.Numerical.roots_complex is
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
   --
   --  Note that the sucess may be sensitive to the choice of x0 and x2.  If you
   --  know that a root exists and get a no_solution error, try different values.
   --
   function mueller(test : test_func; x0, x2 : in out cmplx.complex;
            limit : in out Positive; err : out errors) return cmplx.complex;
end BBS.Numerical.roots_complex;
