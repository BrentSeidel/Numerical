with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Numerics.Generic_Complex_Types;
with Ada.Text_IO;
with BBS.Numerical;
with BBS.Numerical.polynomial_complex;
with BBS.Numerical.roots_complex;

procedure test_poly_cmplx is
   subtype real is Long_Float;
   package cmplx is new Ada.Numerics.Generic_Complex_Types(real);
   package cpoly is new BBS.Numerical.polynomial_complex(cmplx);
   use type cpoly.poly;
   package croot is new BBS.Numerical.roots_complex(cmplx);
   package float_io is new Ada.Text_IO.Float_IO(real);

   p1  : cpoly.poly := ((-1.0, 0.0), (2.0, 0.0), (3.0, 0.0));
   p2  : cpoly.poly := ((3.0, 0.0), (2.0, 0.0), (1.0, 0.0));
   p3  : cpoly.poly(0 .. 2);
   p4  : cpoly.poly(0 .. 4);
   p5  : cpoly.poly(0 .. 2);
   p6  : cpoly.poly(0 .. 3);
   p7  : cpoly.poly(0 .. 2);
   p8  : cpoly.poly(0 .. 2);
   p9  : cpoly.poly(0 .. 1);
   b1  : cpoly.poly := ((1.0, 0.0), (1.0, 0.0));
   b2  : cpoly.poly := ((2.0, 0.0), (1.0, 0.0));
   b3  : cpoly.poly := ((3.0, 0.0), (1.0, 0.0));
   b4  : cpoly.poly := ((4.0, 0.0), (1.0, 0.0));
   b5  : cpoly.poly(0 .. 1);
   b6  : cpoly.poly(0 .. 1);
   d0  : cpoly.poly(0 .. 4);
   d1  : cpoly.poly(0 .. 3);
   d2  : cpoly.poly(0 .. 2);
   d3  : cpoly.poly(0 .. 1);
   x   : cmplx.Complex;
   r   : cmplx.Complex;
   l   : cmplx.Complex;
   u   : cmplx.Complex;
   err : croot.errors;
   iter : Positive;

   function t0(x : cmplx.Complex) return cmplx.Complex is
   begin
      return cpoly.evaluate(d0, x);
   end;

   function t1(x : cmplx.Complex) return cmplx.Complex is
   begin
      return cpoly.evaluate(d1, x);
   end;

   function t2(x : cmplx.Complex) return cmplx.Complex is
   begin
      return cpoly.evaluate(d2, x);
   end;

   procedure cmplx_put(n : cmplx.Complex; fore, aft, exp : Natural) is
   begin
      Ada.Text_IO.Put("(");
      float_io.Put(cmplx.Re(n), fore, aft, exp);
      Ada.Text_IO.Put(",");
      float_io.Put(cmplx.Im(n), fore, aft, exp);
      Ada.Text_IO.Put(")");
   end;

begin
   Ada.Text_IO.Put_Line("Testing some of the numerical routines.");
   Ada.Text_IO.Put_Line("Basic polynomial operations");
   Ada.Text_IO.Put("  p1 = ");
   cpoly.print(p1, 1, 2, 0);
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put("  p2 = ");
   cpoly.print(p2, 1, 2, 0);
   Ada.Text_IO.New_Line;
   p3 := p1 - p2;
   Ada.Text_IO.Put("  p3 = p1-p2 = ");
   cpoly.print(p3, 1, 2, 0);
   Ada.Text_IO.New_Line;
   p4 := p1*p2;
   Ada.Text_IO.Put("  p4 = p1*p2 = ");
   cpoly.print(p4, 1, 2, 0);
   Ada.Text_IO.New_Line;
   p5 := -p2;
   Ada.Text_IO.Put("  p5 = -p2 = ");
   cpoly.print(p5, 1, 2, 0);
   Ada.Text_IO.New_Line;
   cpoly.divide(p4, p1, p8, p9);
   Ada.Text_IO.Put("  p8 = p4/p1 = ");
   cpoly.print(p8, 1, 2, 0);
   Ada.Text_IO.Put(", remainder p9 = ");
   cpoly.print(p9, 1, 2, 0);
   Ada.Text_IO.New_Line;
   --
   Ada.Text_IO.Put_Line("Evaluations of polynomials");
   Ada.Text_IO.Put_Line("   x        p1          p2          p3          p4          p5");
   for i in -15 .. 15 loop
      x := (real(i)*0.1, 0.0);
      cmplx_put(x, 3, 2, 0);
      Ada.Text_IO.Put("  ");
      cmplx_put(cpoly.evaluate(p1, x), 2, 3, 0);
      Ada.Text_IO.Put("  ");
      cmplx_put(cpoly.evaluate(p2, x), 2, 3, 0);
      Ada.Text_IO.Put("  ");
      cmplx_put(cpoly.evaluate(p3, x), 2, 3, 0);
      Ada.Text_IO.Put("  ");
      cmplx_put(cpoly.evaluate(p4, x), 2, 3, 0);
      Ada.Text_IO.Put("  ");
      cmplx_put(cpoly.evaluate(p5, x), 2, 3, 0);
      Ada.Text_IO.New_Line;
   end loop;
   --
   d0 := b1*b2*b3*b4;
   Ada.Text_IO.Put("Find roots of ");
   cpoly.print(d0, 1, 2, 0);
   Ada.Text_IO.New_Line;
   l := (-5.1, 0.0);
   u := (-3.1, 0.0);
   iter := 20;
   r := croot.mueller(t0'Access, l, u, iter, err);
   Ada.Text_IO.Put("  After " & Positive'image(iter) & " iterations, Mueller gives root at ");
   cmplx_put(r, 2, 9, 0);
   Ada.Text_IO.Put(", in range ");
   cmplx_put(l, 2, 6, 0);
   Ada.Text_IO.Put(" to ");
   cmplx_put(u, 2, 6, 0);
   Ada.Text_IO.Put_Line(", with error code " & croot.errors'Image(err));
   b5 := (0 => cmplx."-"(r), 1 => (1.0, 0.0));
   cpoly.divide(d0, b5, d1, b6);
   Ada.Text_IO.Put("Find roots of ");
   cpoly.print(d1, 1, 2, 0);
   Ada.Text_IO.New_Line;
   l := (-4.1, 0.0);
   u := (-0.9, 0.0);
   iter := 13;
   r := croot.mueller(t1'Access, l, u, iter, err);
   Ada.Text_IO.Put("  After " & Positive'image(iter) & " iterations, Mueller gives root at ");
   cmplx_put(r, 2, 9, 0);
   Ada.Text_IO.Put(", in range ");
   cmplx_put(l, 2, 6, 0);
   Ada.Text_IO.Put(" to ");
   cmplx_put(u, 2, 6, 0);
   Ada.Text_IO.Put_Line(", with error code " & croot.errors'Image(err));
   b5 := (0 => cmplx."-"(r), 1 => (1.0, 0.0));
   cpoly.divide(d1, b5, d2, b6);
   Ada.Text_IO.Put("Find roots of ");
   cpoly.print(d2, 1, 2, 0);
   Ada.Text_IO.New_Line;
   l := (-5.1, 0.0);
   u := (0.0, 0.0);
   iter := 13;
   r := croot.mueller(t2'Access, l, u, iter, err);
   Ada.Text_IO.Put("  After " & Positive'image(iter) & " iterations, Mueller gives root at ");
   cmplx_put(r, 2, 9, 0);
   Ada.Text_IO.Put(", in range ");
   cmplx_put(l, 2, 6, 0);
   Ada.Text_IO.Put(" to ");
   cmplx_put(u, 2, 6, 0);
   Ada.Text_IO.Put_Line(", with error code " & croot.errors'Image(err));
   b5 := (0 => cmplx."-"(r), 1 => (1.0, 0.0));
   cpoly.divide(d2, b5, d3, b6);
   Ada.Text_IO.Put("  Last root at ");
   cpoly.print(d3, 1, 2, 0);
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put("  Remainder ");
   cpoly.print(b6, 1, 2, 0);
   Ada.Text_IO.New_Line;
   --
   Ada.Text_IO.Put_Line("Integrals and derivatives");
   p6 := cpoly.integrate(p1, (1.0, 0.0));
   Ada.Text_IO.Put("  P1 = ");
   cpoly.print(p1, 1, 2, 0);
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put("  p6 = Integral of p1 = ");
   cpoly.print(p6, 1, 2, 0);
   Ada.Text_IO.New_Line;
   p7 := cpoly.derivative(p6);
   Ada.Text_IO.Put("  p7 = Derivative of P6 = ");
   cpoly.print(p7, 1, 2, 0);
   Ada.Text_IO.New_Line;
end test_poly_cmplx;
