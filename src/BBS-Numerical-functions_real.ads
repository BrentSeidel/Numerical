--
--  This is a collection of functions that are used by other numerical
--  but aren't provided by the Ada elementary functions package.  Functions
--  that are more specific to a sub group would be in that package.  For
--  example, the normal distribution function is in the Statistics package.
generic
  type F is digits <>;
package BBS.Numerical.functions_real is
   --  Compute the gamma function of a positive number divided by two.
   --  This is initially used by the chi-squared statistics function, but
   --  may have other applications.  The full gamma function will be
   --  implemented when needed.
   --
   --  Note that only gamma of positive and half positive integers are needed,
   --  i.e. 1/2, 1, 3/2, 2, 5/2, ...
   --  This will be easier to implement than the full gamma functions since
   --  gamma(1/2) is sqrt(pi), gamma(1) is 1, and gamma(a+1) is a*gamma(a).
   --
   function gamma2n(n : Positive) return f'Base;
end BBS.Numerical.functions_real;
