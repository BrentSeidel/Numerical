with Ada.Text_IO;
package body BBS.Numerical.ode_real is
   package float_io is new Ada.Text_IO.Float_IO(f'Base);
   -- --------------------------------------------------------------------------
   --  Single step methods
   --
   --  Euler's method
   --
   function euler(tf : test_func; start, initial, step : f'Base) return f'Base is
   begin
      return initial + step*tf(start, initial);
   end;
   --
   --  4th order Runge-Kutta method
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
   -- --------------------------------------------------------------------------
   --  Multistep methods
   --
   --  4th order predictor-corrector (Adams-Bashforth/Adams-Moulton)
   --
   --  Note that this requires extra initial values.  These may be generated
   --  by a fourth order Runge-Kutta or other single step method.
   --
   function ab4(tf : test_func; start, step : f'Base; i0, i1, i2, i3 : f'Base) return f'Base is
   begin
      return i3 + step*(55.0*tf(start, i3) - 59.0*tf(start - step, i2) +
                        37.0*tf(start - 2.0*step, i1) - 9.0*tf(start - 3.0*step, i0))/24.0;
   end;
   --
   function am4(tf : test_func; start, step : f'Base; i0, i1, i2, i3 : f'Base) return f'Base is
   begin
      return i2 + step*(9.0*tf(start + step, i3) + 19.0*tf(start, i2) -
                        5.0*tf(start - step, i1) + tf(start - 2.0*step, i0))/24.0;
   end;
   --
   function abam4(tf : test_func; start, step : f'Base; i0, i1, i2, i3 : f'Base) return f'Base is
      i4 : f'Base := ab4(tf, start, step, i0, i1, i2, i3);
   begin
      return am4(tf, start, step, i1, i2, i3, i4);
   end;
   --
end BBS.Numerical.ode_real;
