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
   function normal(p : F'Base) return F'Base;
   --
   --  Normal distribution with mean and sigma
   --
   function normal(p, mean, sigma : F'Base) return F'Base;
   --
   --  Compute the area under a Normal curve from a to b using the
   --  specified number of steps of Simpson's integration.
   --
   function normal_area(a, b : F'Base; steps : Positive) return F'Base;
end;
