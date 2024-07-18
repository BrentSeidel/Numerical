with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Text_IO;
package body BBS.Numerical.regression is
   package elem is new Ada.Numerics.Generic_Elementary_Functions(f'Base);
   package float_io is new Ada.Text_IO.Float_IO(f);
   --
   --  Compute the simple linear regression to an array of points.
   --
   function simple_linear(d : data_array) return simple_linreg_result is
      Sx   : f'Base := 0.0;  --  Sum of x
      Sy   : f'Base := 0.0;  --  Sum of y
      Sxy  : f'Base := 0.0;  --  Sum of x*y
      Sxx  : f'Base := 0.0;  --  Sum of x*x
      Syy  : f'Base := 0.0;  --  Sum of y*y
      SSxy : f'Base := 0.0;  --  (Sum of x*y) squared
      SSxx : f'Base := 0.0;  --  (Sum of x*x) squared
      SSyy : f'Base := 0.0;
      err  : f'Base;         --  Errors
      SSe  : f'Base := 0.0;  --  Sum of err*err
      a    : f'Base;         --  Y intercept
      b    : f'Base;         --  Slope
      n    : constant f'Base := f'Base(d'Length);
      var  : f'Base;         --  Line variarance
   begin
      for i in d'Range loop
         Sx := Sx + d(i).x;
         Sy := Sy + d(i).y;
         Sxy := Sxy + d(i).x*d(i).y;
         Sxx := Sxx + d(i).x*d(i).x;
         Syy := Syy + d(i).y*d(i).y;
      end loop;
      SSxy := Sxy - (Sx*Sy)/n;
      SSxx := Sxx - (Sx*Sx)/n;
      SSyy := Syy - (Sy*Sy/n);
      a := Sy/n - (Sx/n)*(SSxy/SSxx);
      b := SSxy/SSxx;
      for i in d'Range loop
        err := d(i).y - (a + b*d(i).x);
        SSe := SSe + err*err;
      end loop;
      var := SSe/(n-2.0);
      return (a => a, b => b, SSe => SSe, var => var, Bvar => var/SSxx, cor => SSxy/elem.sqrt(SSxx*SSyy));
   end;
end BBS.Numerical.regression;
