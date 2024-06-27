with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Text_IO;
with BBS.Numerical;
with BBS.Numerical.derivative_real;
with BBS.Numerical.functions_real;
with BBS.Numerical.integration_real;
with BBS.Numerical.interpolation;
with BBS.Numerical.regression;
with BBS.Numerical.Statistics;
package test_cases is
   subtype real is Float;
   package derive is new BBS.Numerical.derivative_real(real);
   package funct is new BBS.Numerical.functions_real(real);
   package integ is new BBS.Numerical.integration_real(real);
   package interp is new BBS.Numerical.interpolation(real);
   package float_io is new Ada.Text_IO.Float_IO(real);
   package elem is new Ada.Numerics.Generic_Elementary_Functions(real);

   procedure test_derive;
   procedure test_func;
   procedure test_integ;
   procedure test_interp;
end;
