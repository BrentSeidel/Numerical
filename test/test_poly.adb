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

   procedure put_poly(p : poly.poly) is
   begin
      for i in reverse p'Range loop
         if p(i) >= 0.0 then
            Ada.Text_IO.Put("+");
         end if;
         float_io.put(p(i), 1, 6, 0);
         Ada.Text_IO.Put("*X^" & Natural'Image(i));
      end loop;
   end;

   p1  : poly.poly := (-1.0, 2.0, 3.0);
   p2  : poly.poly := (3.0, 2.0, 1.0);
   p3  : poly.poly(0 .. 2);
   p4  : poly.poly(0 .. 4);
   p5  : poly.poly(0 .. 2);
   p6  : poly.poly(0 .. 3);
   p7  : poly.poly(0 .. 2);
   p8  : poly.poly(0 .. 2);
   p9  : poly.poly(0 .. 1);
   x   : real;
   r   : real;
   l   : real;
   u   : real;
   err : root.errors;
   iter : Positive;

   function test(x : real) return real is
   begin
      return poly.evaluate(p4, x);
   end;
begin
   Ada.Text_IO.Put_Line("Testing some of the numerical routines.");
   Ada.Text_IO.Put_Line("Basic polynomial operations");
   Ada.Text_IO.Put("  p1 = ");
   put_poly(p1);
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put("  p2 = ");
   put_poly(p2);
   Ada.Text_IO.New_Line;
   p3 := p1 + p2;
   Ada.Text_IO.Put("  p3 = p1+p2 = ");
   put_poly(p3);
   Ada.Text_IO.New_Line;
   p4 := p1*p2;
   Ada.Text_IO.Put("  p4 = p1*p2 = ");
   put_poly(p4);
   Ada.Text_IO.New_Line;
   p5 := (-1.0)*p2;
   Ada.Text_IO.Put("  p5 = -p2 = ");
   put_poly(p5);
   Ada.Text_IO.New_Line;
   poly.divide(p4, p1, p8, p9);
   Ada.Text_IO.Put("  p8 = p4/p1 = ");
   put_poly(p8);
   Ada.Text_IO.Put(", remainder p9 = ");
   put_poly(p9);
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
   Ada.Text_IO.Put_Line("Find a root of P4");
   l := 0.0;
   u := 1.0;
   iter := 13;
   r := root.mueller(test'Access, l, u, iter, err);
   Ada.Text_IO.Put("  After " & Positive'image(iter) & " iterations, Mueller gives root at");
   float_io.Put(r, 2, 9, 0);
   Ada.Text_IO.Put(", in range ");
   float_io.Put(l, 2, 6, 0);
   Ada.Text_IO.Put(" to ");
   float_io.Put(u, 2, 6, 0);
   Ada.Text_IO.Put_Line(", with error code " & root.errors'Image(err));
   --
   Ada.Text_IO.Put_Line("Integrals and derivatives");
   p6 := poly.integrate(p1, 1.0);
   Ada.Text_IO.Put("  P1 = ");
   put_poly(p1);
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put("  p6 = Integral of p1 = ");
   put_poly(p6);
   Ada.Text_IO.New_Line;
   p7 := poly.derivative(p6);
   Ada.Text_IO.Put("  p7 = Derivative of P6 = ");
   put_poly(p7);
   Ada.Text_IO.New_Line;
end test_poly;
