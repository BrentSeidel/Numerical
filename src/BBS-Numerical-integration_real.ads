generic
  type F is digits <>;
package BBS.Numerical.integration_real is
   --
   --  Define a type for the function to integrate.
   --
   type test_func is access function (x : f'Base) return f'Base;
   --
   --  Integrate the provided function between the lower and upper limits using
   --  the composite trapezoid method with the specified number of steps.
   --
   function trapezoid(test : test_func; lower, upper : f'Base; steps : Positive) return f'Base;
   --
   --  Integrate the provided function between the lower and upper limits using
   --  the composite Simpson's rule with the specified number of steps.  Note
   --  that since Simpson's rule evaluates the function in the midpoint of a
   --  segment, the effective number of steps is doubled.
   --
   function simpson(test : test_func; lower, upper : f'Base; steps : Positive) return f'Base;
   --
   --  Integrate the provided function between the lower and upper limits using
   --  adaptive Simpson's integration.
   --
   function adapt_simpson(test : test_func; lower, upper, tol : f'Base;
                          levels : in out Natural) return f'Base;
end;
