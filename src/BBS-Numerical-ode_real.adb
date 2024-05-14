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
   --
   --  4/5th order Runge-Kutta-Fehlberg method
   --
   function rkf(tf : test_func; start, initial, step : f'Base; tol : out f'Base) return f'Base is
      k1 : f'Base;
      k2 : f'Base;
      k3 : f'Base;
      k4 : f'Base;
      k5 : f'Base;
      k6 : f'Base;
      y1 : f'Base;
      y2 : f'Base;
   begin
      k1 := step*tf(start, initial);
      k2 := step*tf(start + step/4.0, initial + k1/4.0);
      k3 := step*tf(start + 3.0*step/8.0, initial + 9.0*k2/32.0);
      k4 := step*tf(start + 12.0*step/13.0, initial + (1932.0*k1 - 7200.0*k2 + 7296.0*k3)/2197.0);
      k5 := step*tf(start + step, initial + 439.0*k1/216.0 - 8.0*k2 + 3680.0*k3/513.0 - 845.0*k4/4104.0);
      k6 := step*tf(start + step/2.0, initial - 8.0*k1/27.0 + 2.0*k2 - 3544.0*k3/2565.0 + 1859.0*k4/4104.0 - 11.0*k5/40.0);
      y1 := initial + 16.0*k1/135.0 + 6656.0*k3/12825.0 + 28561.0*k4/56430.0 - 9.0*k5/50.0 + 2.0*k6/55.0;
      y2 := initial + 25.0*k1/216.0 + 1408.0*k3/2565.0 + 2197.0*k4/4104.0 - k5/5.0;
      tol := abs(y1 - y2);
      return y1;
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
