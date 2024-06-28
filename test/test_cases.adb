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
      sum : Real;
      val : Real;
   begin
      Ada.Text_IO.Put_Line("Testing functions.");
      Ada.Text_IO.Put_Line("  Gamma Function");
      Ada.Text_IO.Put_Line("    N     gamma2n     lngamma2n  exp(lngamma2n)  factorial    lnfact   exp(lnfact)");
      for i in 1 .. 100 loop
         Ada.Text_IO.Put("  ");
         float_io.Put(Real(i), 3, 0, 0);
         Ada.Text_IO.Put("  ");
         float_io.Put(funct.gamma2n(i), 2, 6, 3);
         Ada.Text_IO.Put("  ");
         float_io.Put(funct.lngamma2n(i), 3, 2, 0);
         Ada.Text_IO.Put("  ");
         float_io.Put(elem.exp(funct.lngamma2n(i)), 2, 6, 3);
         Ada.Text_IO.Put("  ");
         float_io.Put(funct.factorial(i), 2, 6, 3);
         Ada.Text_IO.Put("  ");
         float_io.Put(funct.lnfact(i), 3, 2, 0);
         Ada.Text_IO.Put("  ");
         float_io.Put(elem.exp(funct.lnfact(i)), 2, 6, 3);
         Ada.Text_IO.New_Line;
      end loop;
      Ada.Text_IO.Put_Line("N choose K");
      for n in 0 .. 10 loop
         Ada.Text_IO.put("  ");
         float_io.put(real(n), 2, 0, 0);
         Ada.Text_IO.Put("|");
         sum := 0.0;
         for k in 0 .. n loop
            Ada.Text_IO.Put("  ");
            val := funct.nChoosek(n, k);
            float_io.Put(val, 4, 0, 0);
            sum := sum + val;
         end loop;
         Ada.Text_IO.Put("|  ");
         float_io.Put(sum, 4, 0, 0);
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
      p3 : interp.point;
      x  : real;
      y0 : real;
      y1 : real;
      y2 : real;
   begin
      Ada.Text_IO.Put_Line("Testing interpolation");
      p1.x := 1.0;
      p1.y := interp_f1(p1.x);
      p2.x := 2.0;
      p2.y := interp_f1(p2.x);
      p3.x := 3.0;
      p3.y := interp_f1(p3.x);
      Ada.Text_IO.Put_Line("    N      Actual     Linear     Error      Quadratic  Error");
      for i in 0 .. 40 loop
         x := real(i)*0.1;
         y0 := interp_f1(x);
         y1 := interp.linear(p1, p2, x);
         y2 := interp.quadratic(p1, p2, p3, x);
         Ada.Text_IO.Put("  ");
         float_io.put(x, 3, 1, 0);
         Ada.Text_IO.Put("  ");
         float_io.Put(y0, 2, 6, 0);
         Ada.Text_IO.Put("  ");
         float_io.Put(y1, 2, 6, 0);
         Ada.Text_IO.Put("  ");
         float_io.Put(y0 - y1, 2, 6, 0);
         Ada.Text_IO.Put("  ");
         float_io.Put(y2, 2, 6, 0);
         Ada.Text_IO.Put("  ");
         float_io.Put(y0 - y2, 2, 6, 0);
         Ada.Text_IO.New_Line;
      end loop;
   end;
--  ----------------------------------------------------------------------------
   function ode_f1(x : real) return real is
   begin
      return (100.0/(x*x))*elem.sin(10.0/x);
   end;

   function ode_f2(t, y : real) return real is
   begin
      return -y + t + 1.0;
   end;

   function ode_f1sys(t : real; y : ode.params) return real is
   begin
      return -4.0*y(1) + 3.0*y(2) + 6.0;
   end;

   function ode_f2sys(t : real; y : ode.params) return real is
   begin
      return -2.4*y(1) + 1.6*y(2) + 3.6;
   end;

   procedure test_ode is
      type param_array is array(1 .. 2) of real;
      type sys_func is access function (t : real; y : ode.params) return real;

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
      ysys : ode.params(1..2);
      rsys : ode.params(1..2);
      func : constant ode.functs(1 .. 2) := (ode_f1sys'Access, ode_f2sys'Access);
   begin
      Ada.Text_IO.Put_Line("Testing differential equations.");
      Ada.Text_IO.Put_Line("   Time    Euler's  4th Order RK    RKF    RKF Tol  Adams-Bashforth-Moulton");
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
      Ada.Text_IO.Put("  ");
      float_io.Put(0.0, 2, 6, 3);
      Ada.Text_IO.Put("       ----");
      Ada.Text_IO.New_Line;
      for i in 1 .. 20 loop
         ye := ode.euler(ode_f2'Access, t, ye, step);
         yr := ode.rk4(ode_f2'Access, t, yr, step);
         yrkf := ode.rkf(ode_f2'Access, t, yrkf, step, tol);
         if i = 1 then
            y1 := yr;
         elsif i = 2 then
            y2 := yr;
         elsif i = 3 then
            y3 := yr;
         else
            ypc := ode.abam4(ode_f2'Access, t, step, y0, y1, y2, y3);
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
         Ada.Text_IO.Put("  ");
         float_io.Put(tol, 2, 6, 3);
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
      Ada.Text_IO.Put_Line("   Time   RK4-a      RK4-b");
      ysys := (0.0, 0.0);
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
         rsys := ode.rk4s(func, t, ysys, step);
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
   end test_ode;
--  ----------------------------------------------------------------------------
   cpoly_d0  : cpoly.poly(0 .. 4);
   cpoly_d1  : cpoly.poly(0 .. 3);
   cpoly_d2  : cpoly.poly(0 .. 2);
   function cpoly_t0(x : cmplx.Complex) return cmplx.Complex is
   begin
      return cpoly.evaluate(cpoly_d0, x);
   end;

   function cpoly_t1(x : cmplx.Complex) return cmplx.Complex is
   begin
      return cpoly.evaluate(cpoly_d1, x);
   end;

   function cpoly_t2(x : cmplx.Complex) return cmplx.Complex is
   begin
      return cpoly.evaluate(cpoly_d2, x);
   end;

   procedure test_poly_cmplx is
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
      d3  : cpoly.poly(0 .. 1);
      x   : cmplx.Complex;
      r   : cmplx.Complex;
      l   : cmplx.Complex;
      u   : cmplx.Complex;
      err : croot.errors;
      iter : Positive;


   begin
      Ada.Text_IO.Put_Line("Testing some basic complex polynomial routines.");
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
      cpoly_d0 := b1*b2*b3*b4;
      Ada.Text_IO.Put("Find roots of ");
      cpoly.print(cpoly_d0, 1, 2, 0);
      Ada.Text_IO.New_Line;
      l := (-5.1, 0.0);
      u := (-3.1, 0.0);
      iter := 20;
      r := croot.mueller(cpoly_t0'Access, l, u, iter, err);
      Ada.Text_IO.Put("  After " & Positive'image(iter) & " iterations, Mueller gives root at ");
      cmplx_put(r, 2, 9, 0);
      Ada.Text_IO.Put(", in range ");
      cmplx_put(l, 2, 6, 0);
      Ada.Text_IO.Put(" to ");
      cmplx_put(u, 2, 6, 0);
      Ada.Text_IO.Put_Line(", with error code " & croot.errors'Image(err));
      b5 := (0 => cmplx."-"(r), 1 => (1.0, 0.0));
      cpoly.divide(cpoly_d0, b5, cpoly_d1, b6);
      Ada.Text_IO.Put("Find roots of ");
      cpoly.print(cpoly_d1, 1, 2, 0);
      Ada.Text_IO.New_Line;
      l := (-4.1, 0.0);
      u := (-0.9, 0.0);
      iter := 13;
      r := croot.mueller(cpoly_t1'Access, l, u, iter, err);
      Ada.Text_IO.Put("  After " & Positive'image(iter) & " iterations, Mueller gives root at ");
      cmplx_put(r, 2, 9, 0);
      Ada.Text_IO.Put(", in range ");
      cmplx_put(l, 2, 6, 0);
      Ada.Text_IO.Put(" to ");
      cmplx_put(u, 2, 6, 0);
      Ada.Text_IO.Put_Line(", with error code " & croot.errors'Image(err));
      b5 := (0 => cmplx."-"(r), 1 => (1.0, 0.0));
      cpoly.divide(cpoly_d1, b5, cpoly_d2, b6);
      Ada.Text_IO.Put("Find roots of ");
      cpoly.print(cpoly_d2, 1, 2, 0);
      Ada.Text_IO.New_Line;
      l := (-5.1, 0.0);
      u := (0.0, 0.0);
      iter := 13;
      r := croot.mueller(cpoly_t2'Access, l, u, iter, err);
      Ada.Text_IO.Put("  After " & Positive'image(iter) & " iterations, Mueller gives root at ");
      cmplx_put(r, 2, 9, 0);
      Ada.Text_IO.Put(", in range ");
      cmplx_put(l, 2, 6, 0);
      Ada.Text_IO.Put(" to ");
      cmplx_put(u, 2, 6, 0);
      Ada.Text_IO.Put_Line(", with error code " & croot.errors'Image(err));
      b5 := (0 => cmplx."-"(r), 1 => (1.0, 0.0));
      cpoly.divide(cpoly_d2, b5, d3, b6);
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
--  ----------------------------------------------------------------------------
   poly_d0  : poly.poly(0 .. 4);
   poly_d1  : poly.poly(0 .. 3);
   poly_d2  : poly.poly(0 .. 2);

   function poly_t0(x : real) return real is
   begin
      return poly.evaluate(poly_d0, x);
   end;

   function poly_t1(x : real) return real is
   begin
      return poly.evaluate(poly_d1, x);
   end;

   function poly_t2(x : real) return real is
   begin
      return poly.evaluate(poly_d2, x);
   end;

   procedure test_poly is
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
      d3  : poly.poly(0 .. 1);
      x   : real;
      r   : real;
      l   : real;
      u   : real;
      err : root.errors;
      iter : Positive;

   begin
      Ada.Text_IO.Put_Line("Testing some basic real polynomial operations");
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
      poly_d0 := b1*b2*b3*b4;
      Ada.Text_IO.Put("Find roots of ");
      poly.print(poly_d0, 1, 2, 0);
      Ada.Text_IO.New_Line;
      l := -5.1;
      u := -3.1;
      iter := 20;
      r := root.mueller(poly_t0'Access, l, u, iter, err);
      Ada.Text_IO.Put("  After " & Positive'image(iter) & " iterations, Mueller gives root at");
      float_io.Put(r, 2, 9, 0);
      Ada.Text_IO.Put(", in range ");
      float_io.Put(l, 2, 6, 0);
      Ada.Text_IO.Put(" to ");
      float_io.Put(u, 2, 6, 0);
      Ada.Text_IO.Put_Line(", with error code " & root.errors'Image(err));
      b5 := (0 => -r, 1 => 1.0);
      poly.divide(poly_d0, b5, poly_d1, b6);
      Ada.Text_IO.Put("Find roots of ");
      poly.print(poly_d1, 1, 2, 0);
      Ada.Text_IO.New_Line;
      l := -4.1;
      u := -0.9;
      iter := 13;
      r := root.mueller(poly_t1'Access, l, u, iter, err);
      Ada.Text_IO.Put("  After " & Positive'image(iter) & " iterations, Mueller gives root at");
      float_io.Put(r, 2, 9, 0);
      Ada.Text_IO.Put(", in range ");
      float_io.Put(l, 2, 6, 0);
      Ada.Text_IO.Put(" to ");
      float_io.Put(u, 2, 6, 0);
      Ada.Text_IO.Put_Line(", with error code " & root.errors'Image(err));
      b5 := (0 => -r, 1 => 1.0);
      poly.divide(poly_d1, b5, poly_d2, b6);
      Ada.Text_IO.Put("Find roots of ");
      poly.print(poly_d2, 1, 2, 0);
      Ada.Text_IO.New_Line;
      l := -5.1;
      u := 0.0;
      iter := 13;
      r := root.mueller(poly_t2'Access, l, u, iter, err);
      Ada.Text_IO.Put("  After " & Positive'image(iter) & " iterations, Mueller gives root at");
      float_io.Put(r, 2, 9, 0);
      Ada.Text_IO.Put(", in range ");
      float_io.Put(l, 2, 6, 0);
      Ada.Text_IO.Put(" to ");
      float_io.Put(u, 2, 6, 0);
      Ada.Text_IO.Put_Line(", with error code " & root.errors'Image(err));
      b5 := (0 => -r, 1 => 1.0);
      poly.divide(poly_d2, b5, d3, b6);
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
--  ----------------------------------------------------------------------------
   function root_f1(x : real) return real is
   begin
      return (100.0/(x*x))*elem.sin(10.0/x);
   end;

   function root_f2(x : real) return real is
   begin
      return x*x*x + 4.0*x*x - 10.0;
   end;
   --
   --  This function has no real roots - only two complex ones.
   --
   function root_f3(x : cmplx.Complex) return cmplx.Complex is
   begin
      return x*x + 1.0;
   end;

   procedure test_root is
      r   : real;
      l   : real;
      u   : real;
      err : root.errors;
      cerr : croot.errors;
      cr  : cmplx.Complex;
      cl  : cmplx.Complex;
      cu  : cmplx.Complex;
      iter : Positive;
   begin
      Ada.Text_IO.Put_Line("Testing root finding:");
      l := 1.0;
      u := 2.0;
      r := root.bisection(root_f2'Access, l, u, 13, err);
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
      r := root.seacant(root_f2'Access, l, u, 13, err);
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
      iter := 13;
      r := root.mueller(root_f2'Access, l, u, iter, err);
      Ada.Text_IO.Put("  After " & Positive'image(iter) & " iterations, Mueller gives root at");
      float_io.Put(r, 2, 9, 0);
      Ada.Text_IO.Put(", in range ");
      float_io.Put(l, 2, 6, 0);
      Ada.Text_IO.Put(" to ");
      float_io.Put(u, 2, 6, 0);
      Ada.Text_IO.Put_Line(", with error code " & root.errors'Image(err));
      --
      Ada.Text_IO.Put_Line("Complex roots with Mueller's method");
      cl := (0.0, -2.0);
      cu := (0.0, -3.0);
      iter := 13;
      cr := croot.mueller(root_f3'Access, cl, cu, iter, cerr);
      Ada.Text_IO.Put("  After " & Positive'Image(iter) & " iterations, root found at ");
      cmplx_put(cr, 2, 9, 0);
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put("    In range ");
      cmplx_put(cl, 2, 9, 0);
      Ada.Text_IO.Put(" to ");
      cmplx_put(cu, 2, 9, 0);
      Ada.Text_IO.Put_Line(", with error code " & croot.errors'Image(cerr));
   end test_root;
--  ----------------------------------------------------------------------------

   procedure test_stats is
      data : linreg.data_array :=
         ((x => 1.0, y => 1.0),
         (x => 2.0, y => 1.0),
         (x => 3.0, y => 2.0),
         (x => 4.0, y => 2.0),
         (x => 5.0, y => 4.0));
      res  : linreg.simple_linreg_result;
      val : real;
      dof : Positive := 20;
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
      Ada.Text_IO.Put_Line("Probability Distributions");
      Ada.Text_IO.Put_Line("                 Normal                     Chi^2                     Student's T");
      Ada.Text_IO.Put_Line("     X     PDF           CDF           PDF           CDF           PDF           CDF");
      for i in 0 .. 20 loop
         val := real(i)*1.0;
         Ada.Text_IO.Put("  ");
         float_io.Put(val, 2, 2, 0);
         Ada.Text_IO.Put("  ");
         float_io.Put(stat.normal_pdf(val), 1, 6, 3);
         Ada.Text_IO.Put("  ");
         float_io.Put(stat.normal_cdf(0.0, val, 20), 1, 6, 3);
         Ada.Text_IO.Put("  ");
         float_io.Put(stat.chi2_pdf(val, dof), 1, 6, 3);
         Ada.Text_IO.Put("  ");
         float_io.Put(stat.chi2_cdf(0.0, val, dof, 20), 1, 6, 3);
         Ada.Text_IO.Put("  ");
         float_io.Put(stat.studentT_pdf(val, dof), 1, 6, 3);
         Ada.Text_IO.Put("  ");
         float_io.Put(stat.studentT_cdf(0.0, val, dof, 20), 1, 6, 3);
         Ada.Text_IO.Put("  ");
         float_io.Put(stat.poisson_pmf(Natural(i), dof), 1, 6, 3);
         Ada.Text_IO.New_Line;
      end loop;
      Ada.Text_IO.New_Line;
   end test_stats;
--  ----------------------------------------------------------------------------
--  ----------------------------------------------------------------------------
--  ----------------------------------------------------------------------------
--  ----------------------------------------------------------------------------
   procedure cmplx_put(n : cmplx.Complex; fore, aft, exp : Natural) is
   begin
      Ada.Text_IO.Put("(");
      float_io.Put(cmplx.Re(n), fore, aft, exp);
      Ada.Text_IO.Put(",");
      float_io.Put(cmplx.Im(n), fore, aft, exp);
      Ada.Text_IO.Put(")");
   end;
--  ----------------------------------------------------------------------------
end;
