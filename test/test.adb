with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Text_IO;
with BBS.Numerical;
with BBS.Numerical.complex_real;
with BBS.Numerical.integration_real;
with BBS.Numerical.ode_real;
with BBS.Numerical.regression;
with BBS.Numerical.roots_real;
with BBS.Numerical.Statistics;

procedure test is
   subtype real is Float;
   package linreg is new BBS.Numerical.regression(real);
   package integ is new BBS.Numerical.integration_real(real);
   package ode is new BBS.Numerical.ode_real(real);
   package float_io is new Ada.Text_IO.Float_IO(real);
   package elem is new Ada.Numerics.Generic_Elementary_Functions(real);

   function f1(x : real) return real is
   begin
      return (100.0/(x*x))*elem.sin(10.0/x);
   end;

   function f2(t, y : real) return real is
   begin
      return -y + t + 1.0;
   end;

   type param_array is array(1 .. 2) of real;
   type sys_func is access function (t : real; y : ode.params) return real;

   function f1sys(t : real; y : ode.params) return real is
   begin
      return -4.0*y(1) + 3.0*y(2) + 6.0;
   end;

   function f2sys(t : real; y : ode.params) return real is
   begin
      return -2.4*y(1) + 1.6*y(2) + 3.6;
   end;

   data : linreg.data_array :=
      ((x => 1.0, y => 1.0),
       (x => 2.0, y => 1.0),
       (x => 3.0, y => 2.0),
       (x => 4.0, y => 2.0),
       (x => 5.0, y => 4.0));
   res  : linreg.simple_linreg_result;
   y   : real;
   yr  : real;
   yrkf : real;
   ye  : real;
   ypc : real;
   t   : real;
   tol : real;
   y0  : real;
   y1  : real;
   y2  : real;
   y3  : real;
   step : real;
   tsys : ode.params(1..2);
   ysys : ode.params(1..2);
   rsys : ode.params(1..2);
--   func : constant func_array := (f1sys'Access, f2sys'Access);
begin
   Ada.Text_IO.Put_Line("Testing some of the numerical routines.");
   res := linreg.simple_linear(data);
   Ada.Text_IO.Put_Line("Linear regression result:");
   Ada.Text_IO.Put("  a = ");
   float_io.Put(res.a, 2, 3, 0);
   Ada.Text_IO.Put(", b = ");
   float_io.Put(res.b, 2, 3, 0);
   Ada.Text_IO.Put(", SSe = ");
   float_io.Put(res.SSe, 2, 3, 0);
   Ada.Text_IO.Put(", variance = ");
   float_io.Put(res.variance, 2, 3, 0);
   Ada.Text_IO.New_Line;
   --
   Ada.Text_IO.Put_Line("Testing integration:");
   y := integ.trapezoid(f1'Access, 1.0, 3.0, 10);
   Ada.Text_IO.Put("  Trapazoid rule gives ");
   float_io.Put(y, 2, 3, 0);
   Ada.Text_IO.New_Line;
   y := integ.simpson(f1'Access, 1.0, 3.0, 10);
   Ada.Text_IO.Put("  Simpson rule gives ");
   float_io.Put(y, 2, 6, 0);
   Ada.Text_IO.New_Line;
   tol := 1.0e-6;
   y := integ.adapt_simpson(f1'Access, 1.0, 3.0, tol, 8);
   Ada.Text_IO.Put("  Adaptive Simpson gives ");
   float_io.Put(y, 2, 6, 0);
   Ada.Text_IO.Put(" with estimated tolerance ");
   float_io.Put(tol, 2, 6, 0);
   Ada.Text_IO.New_Line;
   --
   Ada.Text_IO.Put_Line("Testing Differential Equations");
   Ada.Text_IO.Put_Line("   Time    Euler's  4th Order RK  RKF  Adams-Bashforth-Moulton");
   yr := 1.0;
   ye := 1.0;
   yrkf := 1.0;
   y0 := 1.0;
   t  := 0.0;
   step := 0.05;
   Ada.Text_IO.Put("  ");
   float_io.Put(0.0, 2, 2, 0);
   Ada.Text_IO.Put("  ");
   float_io.Put(ye, 2, 6, 0);
   Ada.Text_IO.Put("  ");
   float_io.Put(yr, 2, 6, 0);
   Ada.Text_IO.Put("  ");
   float_io.Put(yrkf, 2, 6, 0);
   Ada.Text_IO.Put("       ----");
   Ada.Text_IO.New_Line;
   for i in 1 .. 20 loop
      ye := ode.euler(f2'Access, t, ye, step);
      yr := ode.rk4(f2'Access, t, yr, step);
      yrkf := ode.rkf(f2'Access, t, yrkf, step, tol);
      if i = 1 then
         y1 := yr;
      elsif i = 2 then
         y2 := yr;
      elsif i = 3 then
         y3 := yr;
      else
         ypc := ode.abam4(f2'Access, t, step, y0, y1, y2, y3);
         y0 := y1;
         y1 := y2;
         y2 := y3;
         y3 := ypc;
      end if;
      t := real(i)*step;
      Ada.Text_IO.Put("  ");
      float_io.Put(t, 2, 2, 0);
      Ada.Text_IO.Put("  ");
      float_io.Put(ye, 2, 6, 0);
      Ada.Text_IO.Put("  ");
      float_io.Put(yr, 2, 6, 0);
      Ada.Text_IO.Put("  ");
      float_io.Put(yrkf, 2, 6, 0);
      if i > 3 then
         Ada.Text_IO.Put("    ");
         float_io.Put(ypc, 2, 6, 0);
      else
         Ada.Text_IO.Put("       ----");
      end if;
      Ada.Text_IO.New_Line;
   end loop;
   --
   Ada.Text_IO.Put_Line("Testing Differential Equation Systems");
   ysys := (0.0, 0.0);
   tsys := (0.0, 0.0);
   step := 0.1;
   t := 0.0;
   Ada.Text_IO.Put("  ");
   float_io.Put(0.0, 2, 2, 0);
   Ada.Text_IO.Put("  ");
   float_io.Put(ysys(1), 2, 6, 0);
   Ada.Text_IO.Put("  ");
   float_io.Put(ysys(2), 2, 6, 0);
   Ada.Text_IO.New_Line;
   for i in 1 .. 10 loop
      rsys := ode.rk4s((1 => f1sys'Access, 2 => f2sys'Access),
               tsys, ysys, step);
      ysys := rsys;
      t := real(i)*step;
      Ada.Text_IO.Put("  ");
      float_io.Put(t, 2, 2, 0);
      Ada.Text_IO.Put("  ");
      float_io.Put(ysys(1), 2, 6, 0);
      Ada.Text_IO.Put("  ");
      float_io.Put(ysys(2), 2, 6, 0);
      Ada.Text_IO.New_Line;
   end loop;
end test;
