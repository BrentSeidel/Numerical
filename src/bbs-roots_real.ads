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
   --  This method will fail if during the process, the function values at the
   --  upper and lower bounds are ever equal.  This will cause a divide by zero
   --  error.
   --
   function seacant(test : test_func; lower, upper : in out f; limit : Positive; err : out errors) return f;
   --
   --  The Mueller algorithm uses three points to model a quadratic curve and uses
   --  that to find a candidate root.  This can potentially be used to find complex
   --  roots, however this implementation does not.
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
   function mueller(test : test_func; x0, x2 : in out f; limit : Positive; err : out errors) return f;
end;
