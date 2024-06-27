package body test_cases is

   function derive_f1(x : real) return real is
   begin
      return elem.log(x);
   end;

   procedure test_derive is
      procedure print(f : derive.test_func; x, h : real) is
         y : real;
      begin
         float_io.Put(h, 1, 4, 0);
         Ada.Text_IO.Put("  ");
         y := derive.pt2(f, x, h);
         float_io.Put(y, 1, 8, 0);
         Ada.Text_IO.Put("  ");
         y := derive.pt3a(f, x, h);
         float_io.Put(y, 1, 8, 0);
         Ada.Text_IO.Put("  ");
         y := derive.pt3b(f, x, h);
         float_io.Put(y, 1, 8, 0);
         Ada.Text_IO.Put("  ");
         y := derive.pt5a(f, x, h);
         float_io.Put(y, 1, 8, 0);
         Ada.Text_IO.Put("  ");
         y := derive.pt5b(f, x, h);
         float_io.Put(y, 1, 8, 0);
         Ada.Text_IO.New_Line;
      end;

   begin
      Ada.Text_IO.Put_Line("Testing numerical derivative routines.");
      --
      Ada.Text_IO.Put_Line("  Various derivative functions");
      Ada.Text_IO.Put_Line("  h       pt2         pt3a        pt3b        pt5a        pt5b");
      print(derive_f1'Access, 1.8, 0.1);
      print(derive_f1'Access, 1.8, 0.01);
      print(derive_f1'Access, 1.8, 0.001);
      --
      --  Interestingly for this particular function accuracy using Float
      --  is worse with a step size of 0.0001 for most derivative methods.
      --
      print(derive_f1'Access, 1.8, 0.0001);
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put("  Actual value is: ");
      float_io.Put(1.0/1.8, 1, 8, 0);
      Ada.Text_IO.New_Line;
   end test_derive;
--  ----------------------------------------------------------------------------
   procedure test_func is
   begin
      Ada.Text_IO.Put_Line("Testing functions.");
      Ada.Text_IO.Put_Line("  Gamma Function");
      Ada.Text_IO.Put_Line("    N     gamma2n     lngamma2n  exp(lngamma2n)");
      for i in 1 .. 100 loop
         Ada.Text_IO.Put("  ");
         float_io.Put(Real(i), 3, 0, 0);
         Ada.Text_IO.Put("  ");
         float_io.Put(funct.gamma2n(i), 2, 6, 3);
         Ada.Text_IO.Put("  ");
         float_io.Put(funct.lngamma2n(i), 3, 2, 0);
         Ada.Text_IO.Put("  ");
         float_io.Put(elem.exp(funct.lngamma2n(i)), 2, 6, 3);
         Ada.Text_IO.New_Line;
      end loop;
   end test_func;
--  ----------------------------------------------------------------------------
   function integ_f1(x : real) return real is
   begin
      return (100.0/(x*x))*elem.sin(10.0/x);
   end;

   function integ_f2(t, y : real) return real is
   begin
      return -y + t + 1.0;
   end;

   procedure test_integ is
      y   : real;
      tol : real;
   begin
      Ada.Text_IO.Put_Line("Testing numerical integration routines.");
      --
      Ada.Text_IO.Put_Line("Testing integration:");
      y := integ.trapezoid(integ_f1'Access, 1.0, 3.0, 10);
      Ada.Text_IO.Put("  Trapazoid rule gives ");
      float_io.Put(y, 2, 3, 0);
      Ada.Text_IO.New_Line;
      y := integ.simpson(integ_f1'Access, 1.0, 3.0, 10);
      Ada.Text_IO.Put("  Simpson rule gives ");
      float_io.Put(y, 2, 6, 0);
      Ada.Text_IO.New_Line;
      tol := 1.0e-6;
      y := integ.adapt_simpson(integ_f1'Access, 1.0, 3.0, tol, 8);
      Ada.Text_IO.Put("  Adaptive Simpson gives ");
      float_io.Put(y, 2, 6, 0);
      Ada.Text_IO.Put(" with estimated tolerance ");
      float_io.Put(tol, 2, 6, 0);
      Ada.Text_IO.New_Line;
   end test_integ;
--  ----------------------------------------------------------------------------
   function interp_f1(x : real) return real is
   begin
      return elem.exp(-x*x);
   end;

   procedure test_interp is
      p1 : interp.point;
      p2 : interp.point;
      x  : real;
      y0 : real;
      y1 : real;
   begin
      Ada.Text_IO.Put_Line("Testing interpolation");
      p1.x := 1.0;
      p1.y := interp_f1(p1.x);
      p2.x := 2.0;
      p2.y := interp_f1(p2.x);
      Ada.Text_IO.Put_Line("  N  Actual  Interp  Error");
      for i in 0 .. 30 loop
         x := real(i)*0.1;
         y0 := interp_f1(x);
         y1 := interp.linear(p1, p2, x);
         Ada.Text_IO.Put("  ");
         float_io.put(x, 3, 1, 0);
         Ada.Text_IO.Put("  ");
         float_io.Put(y0, 2, 6, 0);
         Ada.Text_IO.Put("  ");
         float_io.Put(y1, 2, 6, 0);
         Ada.Text_IO.Put("  ");
         float_io.Put(y0 - y1, 2, 6, 0);
         Ada.Text_IO.New_Line;
      end loop;
   end;
end;
