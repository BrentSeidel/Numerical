with Ada.Text_IO;
with Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;
with BBS.Numerical.Integration_real;
package body BBS.Numerical.Statistics is
   package elem is new Ada.Numerics.Generic_Elementary_Functions(f'Base);
   package float_io is new Ada.Text_IO.Float_IO(f'Base);
   package integ is new BBS.Numerical.Integration_real(f'Base);
   --  -----------------------------------------------------------------
   --
   --  Compute the mean of an array of data
   --
   function mean(d : data_array) return F'Base is
      sum : F := 0.0;
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
      sum2 : F := 0.0;
      sum  : F := 0.0;
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
   function normal(p : F'Base) return F'Base is
      scale : constant F'Base := 1.0/elem.Sqrt(2.0*Ada.Numerics.Pi);
   begin
      return scale*elem.Exp(-p*p/2.0);
   end;
   --
   --  Normal distribution with mean and sigma
   --
   function normal(p, mean, sigma : F'Base) return F'Base is
      scale : constant F'Base := 1.0/(sigma*elem.Sqrt(2.0*Ada.Numerics.Pi));
   begin
      return scale*elem.Exp(-(p-mean)*(p-mean)/(2.0*sigma*sigma));
   end;
   --
   --  Compute the area under a Normal curve from a to b using the
   --  specified number of steps of Simpson's integration.
   --
   function normal_area(a, b : F'Base; steps : Positive) return F'Base is
   begin
      return integ.simpson(normal'Access, a, b, steps);
   end;
end;
