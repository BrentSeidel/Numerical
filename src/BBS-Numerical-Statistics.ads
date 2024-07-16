generic
   type F is digits <>;
   --
   --  Note on naming conventions for probability distributions.  For continuous
   --  distribution functions, there are generally two functions.  The one with the
   --  "_pdf" suffix is the probability density function.  The one with the
   --  "_cdf" suffix is the cumulative distribution function.  For discrete functions,
   --  the "_pmf" suffix is for the probability mass function.
   --
package BBS.Numerical.statistics is
   type data_array is array (Integer range <>) of F'Base;
   --  =================================================================
   --  Statistics of a sample of data
   --
   --  Compute the mean of an array of data
   --
   function mean(d : data_array) return F'Base;
   --
   --  Compute the limits of an array of data
   --
   procedure limits(d : data_array; min : out F'Base; max : out F'Base);
   --
   --  Compute the variance (and mean) of an array of data.  Use this,
   --  instead of mean() if you need both values.
   --
   procedure variance(d : data_array; var : out F'Base; mean : out F'Base);
   --  =================================================================
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
   --  -----------------------------------------------------------------
   --  Chi square distribution.
   --  Note that the degrees of freedom (k) must be positive otherwise that
   --  would imply zero or fewer data points.
   --
   function chi2_pdf(x : f'Base; k : Positive) return f'Base;
   --
   function chi2_cdf(a, b : F'Base; k, steps : Positive) return F'Base;
   --  -----------------------------------------------------------------
   --  The student's T distrubution is defined as:
   --         gamma((nu+1)/2                 t*t  -(nu+1)/2
   --  f(t) = ----------------------- * (1 + ---)
   --         sqrt(pi*nu)*gamma(nu/2)         nu
   --
   --  Where nu is the degrees of freedom
   --
   function studentT_pdf(t : f'Base; nu : Positive) return f'Base;
   --
   function studentT_cdf(a, b : F'Base; nu, steps : Positive) return F'Base;
   --  -----------------------------------------------------------------
   --  Exponential distribution.
   --  The PDF is lambda*exp(-lambda*x)
   --  The CDF is 1 - exp(-lambda*x)
   --
   function exp_pdf(x, lambda : f'Base) return f'Base;
   --
   function exp_cdf(x, lambda : f'Base) return f'Base;
   --  -----------------------------------------------------------------
   --  Probability mass function for Poisson distribution.
   --
   function poisson_pmf(k : Natural; lambda : Positive) return f'Base;
   --
   --  =================================================================
   --  Statistical tests
   --
   --  One sample t-test
   --  Given sample mean x_bar, sample std deviation s, and sample size n,
   --  compute the student's T statistic for the difference between x_bar
   --  and the specified mean mu0.
   --
   function studentT_one(x_bar, mu0, s, n : f'Base) return f'Base;
   --
   --  =================================================================
private
   --
   --  This is a bit of a hack to get some functions into a form that can
   --  be integrated.
   --
   deg_freedom : Positive;
   --
   function partial_chi2(x : f'Base) return f'Base;
   function partial_studentT(x : f'Base) return f'Base;
end;
