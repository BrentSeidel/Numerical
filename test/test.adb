with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Text_IO;
with BBS.Numerical;
with BBS.Numerical.complex_real;
with BBS.Numerical.integration_real;
with BBS.Numerical.regression;
with BBS.Numerical.roots_real;
with BBS.Numerical.Statistics;

procedure test is
   package linreg is new BBS.Numerical.regression(Long_Float);
   package integ is new BBS.Numerical.integration_real(Long_Float);
   package float_io is new Ada.Text_IO.Float_IO(Long_Float);
   package elem is new Ada.Numerics.Generic_Elementary_Functions(Long_Float);

   function f1(x : Long_Float) return Long_Float is
   begin
      return (100.0/(x*x))*elem.sin(10.0/x);
   end;

   data : linreg.data_array :=
      ((x => 1.0, y => 1.0),
       (x => 2.0, y => 1.0),
       (x => 3.0, y => 2.0),
       (x => 4.0, y => 2.0),
       (x => 5.0, y => 4.0));
   res  : linreg.simple_linreg_result;
   y : Long_Float;
   i : Positive;
begin
   Ada.Text_IO.Put_Line("Testing some of the numerical routines.");
   res := linreg.simple_linear(data);
   Ada.Text_IO.Put_Line("Linear regression result:");
   Ada.Text_IO.Put("a = ");
   float_io.Put(res.a, 2, 3, 0);
   Ada.Text_IO.Put(", b = ");
   float_io.Put(res.b, 2, 3, 0);
   Ada.Text_IO.Put(", SSe = ");
   float_io.Put(res.SSe, 2, 3, 0);
   Ada.Text_IO.Put(", variance = ");
   float_io.Put(res.variance, 2, 3, 0);
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line("Testing integration:");
   y := integ.trapezoid(f1'Access, 1.0, 3.0, 10);
   Ada.Text_IO.Put("Trapazoid rule gives ");
   float_io.Put(y, 2, 3, 0);
   Ada.Text_IO.New_Line;
   y := integ.simpson(f1'Access, 1.0, 3.0, 10);
   Ada.Text_IO.Put("Simpson rule gives ");
   float_io.Put(y, 2, 3, 0);
   Ada.Text_IO.New_Line;
   i := 10;
   y := integ.adapt_simpson(f1'Access, 1.0, 3.0, 1.0e-6, i);
   Ada.Text_IO.Put("Adaptive Simpson gives ");
   float_io.Put(y, 2, 6, 0);
   Ada.Text_IO.New_Line;
end test;
