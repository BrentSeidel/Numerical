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
   --  -----------------------------------------------------------------
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
   --  -----------------------------------------------------------------
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
   --
   --  Chi squared distribution.
   --  Note that the degrees of freedom (k) must be positive otherwise that
   --  would imply zero or fewer data points.
   --
   --               (x^(k/2-1)*exp(-x/2))
   --  The value us --------------------
   --               (2^(k/2)*gamma(k/2))
   --
   function chi2_pdf(x : f'Base; k : Positive) return f'Base is
   begin
      if x > 0.0 then
         return (elem."**"(x, (f'Base(k)/2.0)-1.0)*elem.Exp(-x/2.0))/
                (elem."**"(2.0, (f'Base(k)/2.0)*funct.gamma2n(k)));
      else
         return 0.0;
      end if;
   end;
   --
   function chi2_cdf(a, b : F'Base; k, steps : Positive) return F'Base is
   begin
      deg_freedom := k;
      return integ.simpson(partial_chi2'Access, a, b, steps);
   end;
   --
   function partial_chi2(x : f'Base) return f'Base is
   begin
      return chi2_pdf(x, deg_freedom);
   end;
end;
