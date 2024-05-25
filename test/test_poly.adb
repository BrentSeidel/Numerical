with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Text_IO;
with BBS.Numerical;
with BBS.Numerical.polynomial_real;
with BBS.Numerical.roots_real;

procedure test_poly is
   subtype real is Long_Float;
   package poly is new BBS.Numerical.polynomial_real(real);
   use type poly.poly;
   package root is new BBS.Numerical.roots_real(real);
   package float_io is new Ada.Text_IO.Float_IO(real);

   p1  : poly.poly := (-1.0, 2.0, 3.0);
   p2  : poly.poly := (3.0, 2.0, 1.0);
   p3  : poly.poly(0 .. 2);
   p4  : poly.poly(0 .. 4);
   p5  : poly.poly(0 .. 2);
   p6  : poly.poly(0 .. 3);
   p7  : poly.poly(0 .. 2);
   p8  : poly.poly(0 .. 2);
   p9  : poly.poly(0 .. 1);
   b1  : poly.poly := (1.0, 1.0);
   b2  : poly.poly := (2.0, 1.0);
   b3  : poly.poly := (3.0, 1.0);
   b4  : poly.poly := (4.0, 1.0);
   b5  : poly.poly(0 .. 1);
   b6  : poly.poly(0 .. 1);
   d0  : poly.poly(0 .. 4);
   d1  : poly.poly(0 .. 3);
   d2  : poly.poly(0 .. 2);
   d3  : poly.poly(0 .. 1);
   x   : real;
   r   : real;
   l   : real;
   u   : real;
   err : root.errors;
   iter : Positive;

   function t0(x : real) return real is
   begin
      return poly.evaluate(d0, x);
   end;

   function t1(x : real) return real is
   begin
      return poly.evaluate(d1, x);
   end;

   function t2(x : real) return real is
   begin
      return poly.evaluate(d2, x);
   end;
begin
   Ada.Text_IO.Put_Line("Testing some of the numerical routines.");
   Ada.Text_IO.Put_Line("Basic polynomial operations");
   Ada.Text_IO.Put("  p1 = ");
   poly.print(p1, 1, 2, 0);
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put("  p2 = ");
   poly.print(p2, 1, 2, 0);
   Ada.Text_IO.New_Line;
   p3 := p1 - p2;
   Ada.Text_IO.Put("  p3 = p1-p2 = ");
   poly.print(p3, 1, 2, 0);
   Ada.Text_IO.New_Line;
   p4 := p1*p2;
   Ada.Text_IO.Put("  p4 = p1*p2 = ");
   poly.print(p4, 1, 2, 0);
   Ada.Text_IO.New_Line;
   p5 := -p2;
   Ada.Text_IO.Put("  p5 = -p2 = ");
   poly.print(p5, 1, 2, 0);
   Ada.Text_IO.New_Line;
   poly.divide(p4, p1, p8, p9);
   Ada.Text_IO.Put("  p8 = p4/p1 = ");
   poly.print(p8, 1, 2, 0);
   Ada.Text_IO.Put(", remainder p9 = ");
   poly.print(p9, 1, 2, 0);
   Ada.Text_IO.New_Line;
   --
   Ada.Text_IO.Put_Line("Evaluations of polynomials");
   Ada.Text_IO.Put_Line("   x        p1          p2          p3          p4          p5");
   for i in -15 .. 15 loop
      x := real(i)*0.1;
      float_io.put(x, 3, 2, 0);
      Ada.Text_IO.Put("  ");
      float_io.put(poly.evaluate(p1, x), 3, 6, 0);
      Ada.Text_IO.Put("  ");
      float_io.put(poly.evaluate(p2, x), 3, 6, 0);
      Ada.Text_IO.Put("  ");
      float_io.put(poly.evaluate(p3, x), 3, 6, 0);
      Ada.Text_IO.Put("  ");
      float_io.put(poly.evaluate(p4, x), 3, 6, 0);
      Ada.Text_IO.Put("  ");
      float_io.put(poly.evaluate(p5, x), 3, 6, 0);
      Ada.Text_IO.New_Line;
   end loop;
   --
   d0 := b1*b2*b3*b4;
   Ada.Text_IO.Put("Find roots of ");
   poly.print(d0, 1, 2, 0);
   Ada.Text_IO.New_Line;
   l := -5.1;
   u := -3.1;
   iter := 20;
   r := root.mueller(t0'Access, l, u, iter, err);
   Ada.Text_IO.Put("  After " & Positive'image(iter) & " iterations, Mueller gives root at");
   float_io.Put(r, 2, 9, 0);
   Ada.Text_IO.Put(", in range ");
   float_io.Put(l, 2, 6, 0);
   Ada.Text_IO.Put(" to ");
   float_io.Put(u, 2, 6, 0);
   Ada.Text_IO.Put_Line(", with error code " & root.errors'Image(err));
   b5 := (0 => -r, 1 => 1.0);
   poly.divide(d0, b5, d1, b6);
   Ada.Text_IO.Put("Find roots of ");
   poly.print(d1, 1, 2, 0);
   Ada.Text_IO.New_Line;
   l := -4.1;
   u := -0.9;
   iter := 13;
   r := root.mueller(t1'Access, l, u, iter, err);
   Ada.Text_IO.Put("  After " & Positive'image(iter) & " iterations, Mueller gives root at");
   float_io.Put(r, 2, 9, 0);
   Ada.Text_IO.Put(", in range ");
   float_io.Put(l, 2, 6, 0);
   Ada.Text_IO.Put(" to ");
   float_io.Put(u, 2, 6, 0);
   Ada.Text_IO.Put_Line(", with error code " & root.errors'Image(err));
   b5 := (0 => -r, 1 => 1.0);
   poly.divide(d1, b5, d2, b6);
   Ada.Text_IO.Put("Find roots of ");
   poly.print(d2, 1, 2, 0);
   Ada.Text_IO.New_Line;
   l := -5.1;
   u := 0.0;
   iter := 13;
   r := root.mueller(t2'Access, l, u, iter, err);
   Ada.Text_IO.Put("  After " & Positive'image(iter) & " iterations, Mueller gives root at");
   float_io.Put(r, 2, 9, 0);
   Ada.Text_IO.Put(", in range ");
   float_io.Put(l, 2, 6, 0);
   Ada.Text_IO.Put(" to ");
   float_io.Put(u, 2, 6, 0);
   Ada.Text_IO.Put_Line(", with error code " & root.errors'Image(err));
   b5 := (0 => -r, 1 => 1.0);
   poly.divide(d2, b5, d3, b6);
   Ada.Text_IO.Put("  Last root at ");
   poly.print(d3, 1, 2, 0);
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put("  Remainder ");
   poly.print(b6, 1, 2, 0);
   Ada.Text_IO.New_Line;
   --
   Ada.Text_IO.Put_Line("Integrals and derivatives");
   p6 := poly.integrate(p1, 1.0);
   Ada.Text_IO.Put("  P1 = ");
   poly.print(p1, 1, 2, 0);
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put("  p6 = Integral of p1 = ");
   poly.print(p6, 1, 2, 0);
   Ada.Text_IO.New_Line;
   p7 := poly.derivative(p6);
   Ada.Text_IO.Put("  p7 = Derivative of P6 = ");
   poly.print(p7, 1, 2, 0);
   Ada.Text_IO.New_Line;
end test_poly;
