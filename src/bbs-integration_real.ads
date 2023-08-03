generic
  type F is digits <>;
package BBS.integration_real is
   type test_func is access function (x : f) return f;
   --
   --  Integrate the provided function between the lower and upper limits using
   --  the composite trapezoid method with the specified number of steps.
   --
   function trapezoid(test : test_func; lower, upper : f; steps : Positive) return f;
   --
   --  Integrate the provided function between the lower and upper limits using
   --  the composite simpson's rule with the specified number of steps.  Note
   --  that since simpson's rule evaluates the function in the midpoint of a
   --  segment, the effective number of steps is doubled.
   --
   function simpson(test : test_func; lower, upper : f; steps : Positive) return f;
   --
end;
