with Ada.Text_IO;
package body BBS.Numerical.regression is
   package float_io is new Ada.Text_IO.Float_IO(f);
   --
   --  Compute the simple linear regression to an array of points.
   --
   function simple_linear(d : data_array) return simple_linreg_result is
      Sx   : f'Base := 0.0;  --  Sum of x
      Sy   : f'Base := 0.0;  --  Sum of y
      Sxy  : f'Base := 0.0;  --  Sum of x*y
      Sxx  : f'Base := 0.0;  --  Sum of x*x
      SSxy : f'Base := 0.0;  --  (Sum of x*y) squared
      SSxx : f'Base := 0.0;  --  (Sum of x*x) squared
      err  : f'Base;         --  Errors
      SSe  : f'Base := 0.0;  --  Sum of err*err
      a    : f'Base;         --  Y intercept
      b    : f'Base;         --  Slope
      n : constant f'Base := f'Base(d'Length);
   begin
      for i in d'Range loop
         Sx := Sx + d(i).x;
         Sy := Sy + d(i).y;
         Sxy := Sxy + d(i).x*d(i).y;
         Sxx := Sxx + d(i).x*d(i).x;
      end loop;
      SSxy := Sxy - (Sx*Sy)/n;
      SSxx := Sxx - (Sx*Sx)/n;
      a := Sy/n - (Sx/n)*(SSxy/SSxx);
      b := SSxy/SSxx;
      for i in d'Range loop
        err := d(i).y - (a + b*d(i).x);
        SSe := SSe + err*err;
      end loop;
      return (a => a, b => b, SSe => SSe, variance => SSe/(n-2.0));
   end;
end BBS.Numerical.regression;
