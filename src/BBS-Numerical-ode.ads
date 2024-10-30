generic
  type F is digits <>;
package BBS.Numerical.ode is
   -- --------------------------------------------------------------------------
   --  Define a type for the function to integrate.
   --
   type test_func is access function (t, y : f'Base) return f'Base;
   --
   --  Define types for systems of differential equations
   --
   type params is array (integer range <>) of f'Base;
   type sys_func is access function (t : f'Base; y : params) return f'Base;
   type functs is array (integer range <>) of sys_func;
   --
   --  Define a type for a 2nd order differential equation
   --
   type func2nd is access function (t, y : f'Base) return f'Base;
   -- --------------------------------------------------------------------------
   --  Single step methods.
   --
   --  For these methods, the following parameters are common:
   --  th      - The function containing the differential equation
   --  start   - The current independent value (usually time) for evaluation
   --  initial - The initial value (or result from the previous step)
   --  step    - The step size
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
   -- --------------------------------------------------------------------------
   --  In most cases with higher order differential equations, you would solve
   --  them as a system of first order equations using, for example, the rk4s
   --  routine above.  For certain second order equations, this method can be
   --  more efficient.  It was first used in 1791 by Jean Baptiste Delambre, but
   --  is named after Carl Stormer who used it in 1907.
   --
   --  There are two functions.  The first is used to start when you have
   --  values for y(0) and y'(0).  The next is called to continue iteration
   --  and requires y(n) and y(n-1).
   --
   function stormer_start(tf : func2nd; start, y0, yp0, step : f'Base) return f'Base;
   function stormer_next(tf : func2nd; start, y0, ym1, step : f'Base) return f'Base;
end BBS.Numerical.ode;
