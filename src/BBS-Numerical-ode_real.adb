with Ada.Text_IO;
package body BBS.Numerical.ode_real is
   package float_io is new Ada.Text_IO.Float_IO(f'Base);
   --
   --  Euler's method
   --
   function euler(tf : test_func; start, initial, step : f'Base) return f'Base is
   begin
      return initial + step*tf(start, initial);
   end;
   --
   --  4th order Runge-Kutta method - single step
   --
   function rk4(tf : test_func; start, initial, step : f'Base) return f'Base is
      k1 : f'Base;
      k2 : f'Base;
      k3 : f'Base;
      k4 : f'Base;
   begin
      k1 := step*tf(start, initial);
      k2 := step*tf(start + step/2.0, initial + k1/2.0);
      k3 := step*tf(start + step/2.0, initial + k2/2.0);
      k4 := step*tf(start + step, initial + k3);
      return initial + (k1 + 2.0*k2 + 2.0*k3 + k4)/6.0;
   end;
end BBS.Numerical.ode_real;
