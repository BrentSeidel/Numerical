generic
  type F is digits <>;
package BBS.Numerical.integration_real is
   --
   --  Define a type for the function to integrate.
   --
   type test_func is access function (x : f'Base) return f'Base;
   --
   --  Integrate the provided function between the lower and upper limits using
   --  the composite midpoint method with the specified number of steps.
   --
   function midpoint(test : test_func; lower, upper : f'Base; steps : Positive) return f'Base;
   --
   --  Integrate the provided function between the lower and upper limits using
   --  the composite trapezoid method with the specified number of steps.
   --
   function trapezoid(test : test_func; lower, upper : f'Base; steps : Positive) return f'Base;
   --
   --  Integrate the provided function between the lower and upper limits using
   --  adaptive trapeziod integration.  A requested tolerance is input and
   --  returned as an estimated value.
   --
   function adapt_trap(test : test_func; lower, upper : f'Base; tol : in out f'Base;
                          levels : Natural) return f'Base;
   --
   --  Integrate the provided function between the lower and upper limits using
   --  the composite Simpson's rule with the specified number of steps.  Note
   --  that since Simpson's rule evaluates the function in the midpoint of a
   --  segment, the effective number of steps is doubled.
   --
   function simpson(test : test_func; lower, upper : f'Base; steps : Positive) return f'Base;
   --
   --  Integrate the provided function between the lower and upper limits using
   --  adaptive Simpson's integration.  A requested tolerance is input and
   --  returned as an estimated value.
   --
   function adapt_simpson(test : test_func; lower, upper : f'Base; tol : in out f'Base;
                          levels : Natural) return f'Base;
   --
   --  Integrate the provided function between the lower and upper limits using
   --  Romberg's method.  This repeatedly uses the trapezoid method and applies
   --  Richardson extrapolation to arrive at the final answer.
   --
   function romberg(test : test_func; lower, upper : f'Base; steps : Positive) return f'Base;
end;
