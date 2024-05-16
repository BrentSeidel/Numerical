with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Text_IO;
with BBS.Numerical;
with BBS.Numerical.integration_real;

procedure test_integ is
   subtype real is Float;
   package integ is new BBS.Numerical.integration_real(real);
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

   y   : real;
   tol : real;
begin
   Ada.Text_IO.Put_Line("Testing some of the numerical routines.");
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
end test_integ;
