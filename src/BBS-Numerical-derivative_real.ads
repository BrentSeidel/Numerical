generic
  type F is digits <>;
package BBS.Numerical.derivative_real is
   -- --------------------------------------------------------------------------
   --  Define a type for the function to integrate.
   --
   type test_func is access function (x : f'Base) return f'Base;
   -- --------------------------------------------------------------------------
   --  WARNING:
   --  The calculations here may involve adding small numbers to large
   --  numbers and taking the difference of two nearly equal numbers.
   --  The world of computers is not the world of mathematics where
   --  numbers have infinite precision.  If you aren't careful, you
   --  can get into a situation where (x + h) = x, or f(x) = f(x +/- h).
   --  For example assume that the float type has 6 digits.  Then, if
   --  x is 1,000,000 and h is 1, adding x and h is a wasted operation.
   --
   --  Two point formula.  Use h > 0 for forward-difference and h < 0
   --  for backward-difference.  Derivative calculated at point x.  While
   --  this is the basis for defining the derivative in calculus, it
   --  generally shouldn't be used.
   --
   function pt2(f1 : test_func; x, h : f'Base) return f'Base;
   --
   --  Three point formulas.
   --
   function pt3a(f1 : test_func; x, h : f'Base) return f'Base;
   function pt3b(f1 : test_func; x, h : f'Base) return f'Base;
   --
   --  Five point formulas.
   --
   function pt5a(f1 : test_func; x, h : f'Base) return f'Base;
   function pt5b(f1 : test_func; x, h : f'Base) return f'Base;
end BBS.Numerical.derivative_real;
