generic
  type F is digits <>;
package BBS.roots_real is
   type errors is (none, bad_args, no_solution);
   type test_func is access function (x : f) return f;

   --
   --  If there is an odd number of roots between the lower and upper limits,
   --  the bisection algorithm will always converge.  Each iteration simply halves
   --  the interval between the limits, keeping a root between them.  The result
   --  is the midpoint of the final interval after the specified number of iterations.
   --
   --  The lower and upper values are updated during the iterations to provide an
   --  interval containing the root.
   --
   function bisection(test : test_func; lower, upper : in out f; limit : Positive; err : out errors) return f;
   --
   --  If there is an odd number of roots between the lower and upper limits,
   --  the secant method will always converge.  Each stage of the iteration
   --  identifies a point where a line between the lower and upper values crosses
   --  the axis.  This point is used instead of the midpoint in the bisection
   --  algorithm.
   --
   --  Depending on the function, this can converge to a root much faster than
   --  the bisection algorithm.  On the other had, it can also converge much
   --  slower.
   --
   --  The lower and upper values are updated during the iterations to provide an
   --  interval containing the root.  If the root is exact, the lower and upper
   --  values are equal to the returned value.
   --
   function seacant(test : test_func; lower, upper : in out f; limit : Positive; err : out errors) return f;
end;
