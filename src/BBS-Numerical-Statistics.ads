generic
   type F is digits <>;
package BBS.Numerical.statistics is
   type data_array is array (Integer range <>) of F'Base;
   --  -----------------------------------------------------------------
   --  Statistics of a sample of data
   --
   --  Compute the mean of an array of data
   --
   function mean(d : data_array) return F'Base;
   --
   --  Compute the median of an array of data
   --
   procedure limits(d : data_array; min : out F'Base; max : out F'Base);
   --
   --  Compute the variance (and mean) of an array of data.  Use this,
   --  instead of mean() if you need both values.
   --
   procedure variance(d : data_array; var : out F'Base; mean : out F'Base);
   --  -----------------------------------------------------------------
   --  Distributions
   --
   --  Standard Normal distribution
   --
   function normal_pdf(p : F'Base) return F'Base;
   --
   --  Normal distribution with mean and sigma
   --
   function normal_pdf(p, mean, sigma : F'Base) return F'Base;
   --
   --  Compute the area under a Normal curve from a to b using the
   --  specified number of steps of Simpson's integration.  Note that the
   --  normal PDF is symetrical so the value from 0 to infinity is exactly
   --  0.5.  Calculation of "a" to infinity can be done by taking 0.5
   --  minus the value from 0 to "a".
   --
   function normal_cdf(a, b : F'Base; steps : Positive) return F'Base;
   --
   --  Chi square distribution.
   --  Note that the degrees of freedom (k) must be positive otherwise that
   --  would imply zero or fewer data points.
   --
   function chi2_pdf(x : f'Base; k : Positive) return f'Base;
   --
   function chi2_cdf(a, b : F'Base; k, steps : Positive) return F'Base;
   --
   --  *** Under development - do not use yet. ***
   --  Perform a chi^2 test on two sets of data
   --
   --  This calculates alpha from the expected and observed data sets and then
   --  computes the chi^2 cdf value from alpha and the degrees of freedom.  This
   --  is the value that is returned.  The larger this value, the more likely the
   --  data sets are different.
   --
   function chi2_test(expected, observed : data_array; k : Positive) return F'Base
     with pre => ((expected'First = observed'First) and (expected'Last = observed'Last));
   --
   --  The student's T distrubution is defined as:
   --         gamma((nu+1)/2                 t*t  -(nu+1)/2
   --  f(t) = ----------------------- * (1 + ---)
   --         sqrt(pi*nu)*gamma(nu/2)         nu
   --
   --  Where nu is the degrees of freedom
   --
   function studentT_pdf(t : f'Base; nu : Positive) return f'Base;
   --
private
   --
   --  This is a bit of a hack to get a chi square function that can
   --  be integrated.
   --
   deg_freedom : Positive;
   --
   function partial_chi2(x : f'Base) return f'Base;
end;
