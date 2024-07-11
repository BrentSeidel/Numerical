generic
  type F is digits <>;
package BBS.Numerical.ode is
   -- --------------------------------------------------------------------------
   --  Define a type for the function to integrate.
   --
   type test_func is access function (t, y : f'Base) return f'Base;
   type params is array (integer range <>) of f'Base;
   type sys_func is access function (t : f'Base; y : params) return f'Base;
   type functs is array (integer range <>) of sys_func;
   -- --------------------------------------------------------------------------
   --  Single step methods
   --
   --  Euler's method (It is not recommended to use this.  It was included only
   --  for illustration and debugging purposes.)
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
   -- --------------------------------------------------------------------------
   --  Systems of differential equation methods
   --
   --  4th order Runge-Kutta method
   --
   --  Note that the arrays for tf, start, and initial must have the same bounds.
   --
   function rk4s(tf : functs; start : f'Base; initial : params; step : f'Base) return params
      with pre => (tf'First = initial'First) and (tf'Last = initial'Last);
end BBS.Numerical.ode;
