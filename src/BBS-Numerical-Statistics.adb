with Ada.Text_IO;
with Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;
with BBS.Numerical.functions_real;
with BBS.Numerical.Integration_real;
package body BBS.Numerical.Statistics is
   package elem is new Ada.Numerics.Generic_Elementary_Functions(f'Base);
   use type f'Base;
   package float_io is new Ada.Text_IO.Float_IO(f'Base);
   package integ is new BBS.Numerical.Integration_real(f'Base);
   package funct is new BBS.Numerical.functions_real(f'Base);
   --  =================================================================
   --  Statistics of a sample of data
   --
   --  Compute the mean of an array of data
   --
   function mean(d : data_array) return F'Base is
      sum : F'Base := 0.0;
   begin
      for i in d'Range loop
         sum := sum + d(i);
      end loop;
      return sum/F(d'Length);
   end;
   --
   --  Compute the limits of an array of data
   --
   procedure limits(d : data_array; min : out F'Base; max : out F'Base) is
   begin
      min := d(d'First);
      max := d(d'First);
      for i in (d'First + 1) .. d'Last loop
         if d(i) > max then
            max := d(i);
         end if;
         if d(i) < min then
            min := d(i);
         end if;
      end loop;
   end;
   --
   --  Compute the variance (and mean) of an array of data.  Use this,
   --  instead of mean() if you need both values.
   --
   procedure variance(d : data_array; var : out F'Base; mean : out F'Base) is
      sum2 : F'Base := 0.0;
      sum  : F'Base := 0.0;
   begin
      for i in d'Range loop
         sum  := sum + d(i);
         sum2 := sum2 + (d(i)*d(i));
      end loop;
      mean := sum/F(d'Length);
      var  := (sum2 - sum*sum/F'Base(d'Length))/F'Base(d'Length - 1);
   end;
   --  =================================================================
   --  Distributions
   --
   --  Normal distribution
   --
   function normal_pdf(p : F'Base) return F'Base is
      scale : constant F'Base := 1.0/elem.Sqrt(2.0*Ada.Numerics.Pi);
   begin
      return scale*elem.Exp(-p*p/2.0);
   end;
   --
   --  Normal distribution with mean and sigma
   --
   function normal_pdf(p, mean, sigma : F'Base) return F'Base is
      scale : constant F'Base := 1.0/(sigma*elem.Sqrt(2.0*Ada.Numerics.Pi));
   begin
      return scale*elem.Exp(-(p-mean)*(p-mean)/(2.0*sigma*sigma));
   end;
   --
   --  Compute the area under a Normal curve from a to b using the
   --  specified number of steps of Simpson's integration.
   --
   function normal_cdf(a, b : F'Base; steps : Positive) return F'Base is
   begin
      return integ.simpson(normal_pdf'Access, a, b, steps);
   end;
   --  -----------------------------------------------------------------
   --  Chi squared distribution.
   --  Note that the degrees of freedom (k) must be positive otherwise that
   --  would imply zero or fewer data points.
   --
   --               (x^(k/2-1)*exp(-x/2))
   --  The value is --------------------
   --               (2^(k/2)*gamma(k/2))
   --
   --  For k > about 70, gamma(k/2) will overflow Float.  So for more
   --  degrees of freedom, need to use logarithms to do the calculations.
   --  Other large numbers from the exponents will also mostly cancel out.
   --
   --  The value is exp[(log(x)*(k/2-1) - x/2) - (log(2)*(k/2) + log(gamma(k/2)))]
   --
   --  This function uses direct computation for k less than 50 otherwise it uses
   --  log/exp computation.  If needed, the threshold can be changed - 50 is arbitrary.
   --
   function chi2_pdf(x : f'Base; k : Positive) return f'Base is
      part1 : f'Base;
      part2 : f'Base;
   begin
      if x > 0.0 then
         if k < 50 then
            return (elem."**"(x, (f'Base(k)/2.0)-1.0)*elem.Exp(-x/2.0))/
                   (elem."**"(2.0, (f'Base(k)/2.0))*funct.gamma2n(k));
         else
            part1 := elem.log(x)*((f'Base(k)/2.0)-1.0) - (x/2.0);
            part2 := elem.log(2.0)*(f'Base(k)/2.0) + funct.lngamma2n(k);
            return elem.exp(part1 - part2);
         end if;
      else
         return 0.0;
      end if;
   end;
   --
   function chi2_cdf(a, b : F'Base; k, steps : Positive) return F'Base is
      tol : f'Base := 0.0001;
   begin
      deg_freedom := k;
      return integ.adapt_simpson(partial_chi2'Access, a, b, tol, steps);
   end;
   --
   --  This uses the global deg_freedom so that it can be called as a function of
   --  one variable by the integration routine.
   --
   function partial_chi2(x : f'Base) return f'Base is
   begin
      return chi2_pdf(x, deg_freedom);
   end;
   --  -----------------------------------------------------------------
   --  The student's T distrubution is defined as:
   --         gamma((nu+1)/2                 t*t  -(nu+1)/2
   --  f(t) = ----------------------- * (1 + ---)
   --         sqrt(pi*nu)*gamma(nu/2)         nu
   --
   --  Where nu is the degrees of freedom
   --
   function studentT_pdf(t : f'Base; nu : Positive) return f'Base is
      part1 : f'Base := funct.lngamma2n(nu+1);
      part2 : f'Base := elem.log(elem.Sqrt(Ada.Numerics.Pi*f'Base(nu))) +
        funct.lngamma2n(nu);
      part3 : f'Base := elem.Log(1.0 + (t*t)/f'Base(nu))*(-(f'Base(nu+1)/2.0));
   begin
      return elem.Exp(part1 + part3 - part2);
   end;
   --
   --  This uses the global deg_freedom so that it can be called as a function of
   --  one variable by the integration routine.
   --
   function partial_studentT(x : f'Base) return f'Base is
   begin
      return studentT_pdf(x, deg_freedom);
   end;
   --
   function studentT_cdf(a, b : F'Base; nu, steps : Positive) return F'Base is
      tol : f'Base := 0.0001;
   begin
      deg_freedom := nu;
      return integ.adapt_simpson(partial_studentT'Access, a, b, tol, steps);
   end;
   --  -----------------------------------------------------------------
   --  Exponential distribution.
   --  The PDF is lambda*exp(-lambda*x)
   --  The CDF is 1 - exp(-lambda*x)
   --
   function exp_pdf(x, lambda : f'Base) return f'Base is
   begin
      if x < 0.0 then
         return 0.0;
      else
         return lambda*elem.exp(-lambda*x);
      end if;
   end;
   --
   function exp_cdf(x, lambda : f'Base) return f'Base is
   begin
      if x < 0.0 then
         return 0.0;
      else
         return 1.0 - elem.exp(-lambda*x);
      end if;
   end;
   --  -----------------------------------------------------------------
   --  Probability mass function for Poisson distribution.
   --         lambda^k * exp(-lambda)
   --  p(k) = -----------------------
   --                  k!
   --
   function poisson_pmf(k : Natural; lambda : Positive) return f'Base is
      part1 : f'Base := elem.log(f'Base(lambda))*f'Base(k) - f'Base(lambda);
      part2 : f'Base := funct.lnfact(k);
   begin
      return elem.exp(part1 - part2);
   end;
   --  =================================================================
   --  Statistical tests
   --
   --  One sample t-test
   --  Given sample mean x_bar, sample std deviation s, and sample size n,
   --  compute the student's T statistic for the difference between x_bar
   --  and the specified mean mu0.
   --
   function studentT_one(x_bar, mu0, s, n : f'Base) return f'Base is
      diff : constant f'Base := x_bar - mu0;
      bottom : constant f'Base := s/elem.sqrt(n);
   begin
      return diff/bottom;
   end;
   --  =================================================================
end;
