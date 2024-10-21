--
--  This is a collection of functions that are used by other numerical
--  but aren't provided by the Ada elementary functions package.  Functions
--  that are more specific to a sub group would be in that package.  For
--  example, the normal distribution function is in the Statistics package.
generic
  type F is digits <>;
package BBS.Numerical.functions is
   --
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
   --  Note that for Float, gamma2n overflows with n > 70.
   --
   function gamma2n(n : Positive) return f'Base;
   --
   --  Return the natural logarithm of gamma2n.  This allows larger
   --  values of n without overflow.
   --
   function lngamma2n(n : Positive) return f'Base;
   --
   --  Natural log of Gamma function based on based on Lanczos approximation
   --  given in Numerical Recipes in C.  This should work for all real numbers
   --  greater than zero.
   --
   function lngamma(n : f'Base) return f'Base
      with pre => (n > 0.0);
   --
   --  Regularized incomplete upper Gamma function.
   --
   function gammaP(a, x : f'Base) return f'Base
      with pre => ((a > 0.0) and (x > 0.0));
   --
   --  Regularized incomplete lower Gamma function.
   --
   function gammaQ(a, x : f'Base) return f'Base
      with pre => ((a > 0.0) and (x > 0.0));
   --
   --  Compute the factorial of a number.  This will probably overflow Float at
   --  around n = 35.
   --
   function factorial(n : Natural) return f'Base;
   --
   --  Compute the natural log of the factorial.  This will allow much larger
   --  values of n before overflowing.
   --
   function lnfact(n : Natural) return f'Base;
   --
   --  Compute the binomial coefficient - n choose k.  Note that the result is
   --  an integer value, but f'Base is used to allow greater range.
   --
   --   (n)      n!
   --   ( ) = --------
   --   (k)   k!(n-k)!
   --
   --  Expanding the factorials and cancelling terms leads to the following
   --  formula.  Note that since it is symmetrical with respect to k, the limit
   --  for the product can be the lesser of k or (n-k).
   --
   --   (n)    k   n + 1 - i
   --   ( ) = PROD ---------
   --   (k)   i=1      i
   --
   --  This also allows larger values of n and k without overflowing.
   --
   function nChoosek(n, k : Natural) return f'Base
     with pre => (n >= k);
   --
end BBS.Numerical.functions;
