generic
  type F is digits <>;
package BBS.Numerical.ode_real is
   --
   --  Define a type for the function to integrate.
   --
   type test_func is access function (t, y : f'Base) return f'Base;
   --
   --  Euler's method
   --
   function euler(tf : test_func; start, initial, step : f'Base) return f'Base;
   --
   --  4th order Runge-Kutta method - single step
   --
   function rk4(tf : test_func; start, initial, step : f'Base) return f'Base;
   --
end BBS.Numerical.ode_real;
