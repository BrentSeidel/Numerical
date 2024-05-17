with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Text_IO;
with BBS.Numerical;
with BBS.Numerical.complex_real;
with BBS.Numerical.roots_real;
with BBS.Numerical.roots_complex;

procedure test_root is
   subtype real is Long_Float;
   package root is new BBS.Numerical.roots_real(real);
   package croot is new BBS.Numerical.roots_complex(real);
   use type croot.cmplx.complex;
   package float_io is new Ada.Text_IO.Float_IO(real);
   package elem is new Ada.Numerics.Generic_Elementary_Functions(real);

   function f1(x : real) return real is
   begin
      return (100.0/(x*x))*elem.sin(10.0/x);
   end;

   function f2(x : real) return real is
   begin
      return x*x*x + 4.0*x*x - 10.0;
   end;
   --
   --  This function has no real roots - only two complex ones.
   --
   function f3(x : croot.cmplx.Complex) return croot.cmplx.Complex is
   begin
      return x*x + croot.cmplx.one;
   end;

   r   : real;
   l   : real;
   u   : real;
   err : root.errors;
   cerr : croot.errors;
   cr  : croot.cmplx.Complex;
   cl  : croot.cmplx.Complex;
   cu  : croot.cmplx.Complex;
begin
   Ada.Text_IO.Put_Line("Testing some of the numerical routines.");
   --
   Ada.Text_IO.Put_Line("Testing root finding:");
   l := 1.0;
   u := 2.0;
   r := root.bisection(f2'Access, l, u, 13, err);
   Ada.Text_IO.Put("  Bisection gives root at");
   float_io.Put(r, 2, 9, 0);
   Ada.Text_IO.Put(", in range ");
   float_io.Put(l, 2, 6, 0);
   Ada.Text_IO.Put(" to ");
   float_io.Put(u, 2, 6, 0);
   Ada.Text_IO.Put_Line(", with error code " & root.errors'Image(err));
   --
   l := 1.0;
   u := 2.0;
   r := root.seacant(f2'Access, l, u, 13, err);
   Ada.Text_IO.Put("  Seacant gives root at");
   float_io.Put(r, 2, 9, 0);
   Ada.Text_IO.Put(", in range ");
   float_io.Put(l, 2, 6, 0);
   Ada.Text_IO.Put(" to ");
   float_io.Put(u, 2, 6, 0);
   Ada.Text_IO.Put_Line(", with error code " & root.errors'Image(err));
   --
   l := 1.0;
   u := 2.0;
   r := root.mueller(f2'Access, l, u, 13, err);
   Ada.Text_IO.Put("  Mueller gives root at");
   float_io.Put(r, 2, 9, 0);
   Ada.Text_IO.Put(", in range ");
   float_io.Put(l, 2, 6, 0);
   Ada.Text_IO.Put(" to ");
   float_io.Put(u, 2, 6, 0);
   Ada.Text_IO.Put_Line(", with error code " & root.errors'Image(err));
   --
   Ada.Text_IO.Put_Line("Complex roots with Mueller's method");
   cl := (r => 1.0, i => 0.0);
   cu := (r => 2.0, i => 0.0);
   cr := croot.mueller(f3'Access, cl, cu, 13, cerr);
   Ada.Text_IO.Put("  Root at ");
   cr.print(2, 9, 0);
   Ada.Text_IO.Put(", in range ");
   cl.print(2, 9, 0);
   Ada.Text_IO.Put(" to ");
   cu.print(2, 9, 0);
   Ada.Text_IO.Put_Line(", with error code " & croot.errors'Image(cerr));
end test_root;
