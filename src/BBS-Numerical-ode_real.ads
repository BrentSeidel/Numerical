generic
  type F is digits <>;
package BBS.Numerical.ode_real is
   -- --------------------------------------------------------------------------
   --  Define a type for the function to integrate.
   --
   type test_func is access function (t, y : f'Base) return f'Base;
   -- --------------------------------------------------------------------------
   --  Single step methods
   --
   --  Euler's method
   --
   function euler(tf : test_func; start, initial, step : f'Base) return f'Base;
   --
   --  4th order Runge-Kutta method
   --
   function rk4(tf : test_func; start, initial, step : f'Base) return f'Base;
   --
   --  4/5th order Runge-Kutta-Fehlberg method
   --
   function rkf(tf : test_func; start, initial, step : f'Base; tol : out f'Base) return f'Base;
   -- --------------------------------------------------------------------------
   --  Multistep methods
   --
   --  4th order predictor-corrector (Adams-Bashforth/Adams-Moulton)
   --
   function ab4(tf : test_func; start, step : f'Base; i0, i1, i2, i3 : f'Base) return f'Base;
   function am4(tf : test_func; start, step : f'Base; i0, i1, i2, i3 : f'Base) return f'Base;
   function abam4(tf : test_func; start, step : f'Base; i0, i1, i2, i3 : f'Base) return f'Base;
end BBS.Numerical.ode_real;
