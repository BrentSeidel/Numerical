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
   function chi2_exp(x : f'Base; k : Positive) return f'Base;
   --
   function chi2_cdf(a, b : F'Base; k, steps : Positive) return F'Base;
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
