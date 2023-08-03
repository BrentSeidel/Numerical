generic
  type F is digits <>;
package BBS.integration_real is
   type test_func is access function (x : f) return f;
   --
   --  Integrate the provided function between the lower and upper limits using
   --  the composite trapezoid method with the specified number of steps.
   --
   function trapezoid(test : test_func; lower, upper : f; steps : Positive) return f;
end;
