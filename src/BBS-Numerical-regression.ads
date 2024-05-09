generic
  type F is digits <>;
package BBS.Numerical.regression is

   type point is record
      x : f'Base;
      y : f'Base;
   end record;
   type data_array is array (Integer range <>) of point;

   --
   --  Holds the return data from the simple linear regression function.
   --  It contains coefficients for the line y = a + bX.
   type simple_linreg_result is record
      a   : f'Base;  --  Y intercept
      b   : f'Base;  --  Slope
      SSe : f'Base;  --  Sum of square errors
      variance : f'Base;  --  Variance
   end record;

   function simple_linear(d : data_array) return simple_linreg_result;

end BBS.Numerical.regression;
