with BBS.Numerical.plot;
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
      t   : Real;
   begin
      Ada.Text_IO.Put_Line("Testing functions.");
      Ada.Text_IO.Put_Line("  Gamma Function");
      Ada.Text_IO.Put_Line("    N     gamma2n     lngamma2n  exp(lngamma2n)" &
               " lngamma  exp(lngamma)   factorial    lnfact   exp(lnfact)");
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
         float_io.Put(funct.lngamma(Real(i)/2.0), 3, 2, 0);
         Ada.Text_IO.Put("  ");
         float_io.Put(elem.exp(funct.lngamma(Real(i)/2.0)), 2, 6, 3);
         Ada.Text_IO.Put("  ");
         float_io.Put(funct.factorial(i), 2, 6, 3);
         Ada.Text_IO.Put("  ");
         float_io.Put(funct.lnfact(i), 3, 2, 0);
         Ada.Text_IO.Put("  ");
         float_io.Put(elem.exp(funct.lnfact(i)), 2, 6, 3);
         Ada.Text_IO.New_Line;
      end loop;
      Ada.Text_IO.Put_Line("  Normalized Incomplete Gamma Functions");
      Ada.Text_IO.Put_Line("    N    P(0.5, N)  P(1.0, N)  P(3.0, N)  P(10.0, N)" &
                           " Q(0.5, N)  Q(1.0, N)  Q(3.0, N)  Q(10.0, N)  erf(N)   erfc(N)");
      for i in 0 .. 99 loop
         t := Real(i)/10.0;
         Ada.Text_IO.Put("  ");
         float_io.Put(t, 3, 0, 0);
         Ada.Text_IO.Put("  ");
         float_io.Put(funct.gammaP(0.5, t), 3, 4, 0);
         Ada.Text_IO.Put("   ");
         float_io.Put(funct.gammaP(1.0, t), 3, 4, 0);
         Ada.Text_IO.Put("   ");
         float_io.Put(funct.gammaP(3.0, t), 3, 4, 0);
         Ada.Text_IO.Put("   ");
         float_io.Put(funct.gammaP(10.0, t), 3, 4, 0);
         Ada.Text_IO.Put("   ");
         float_io.Put(funct.gammaQ(0.5, t), 3, 4, 0);
         Ada.Text_IO.Put("   ");
         float_io.Put(funct.gammaQ(1.0, t), 3, 4, 0);
         Ada.Text_IO.Put("   ");
         float_io.Put(funct.gammaQ(3.0, t), 3, 4, 0);
         Ada.Text_IO.Put("   ");
         float_io.Put(funct.gammaQ(10.0, t), 3, 4, 0);
         Ada.Text_IO.Put("  ");
         float_io.Put(funct.erf(t), 3, 4, 0);
         Ada.Text_IO.Put(" ");
         float_io.Put(funct.erfc(t), 3, 4, 0);
         Ada.Text_IO.New_Line;
      end loop;
      Ada.Text_IO.Put_Line("  Beta Function");
      Ada.Text_IO.Put_Line("    N     B(N, N)  ln(B(N,N))");
      for i in 1 .. 100 loop
         t := Real(i)/10.0;
         Ada.Text_IO.Put("  ");
         float_io.Put(t, 3, 0, 0);
         Ada.Text_IO.Put("  ");
         float_io.Put(funct.beta(t, t), 3, 4, 0);
         Ada.Text_IO.Put("  ");
         float_io.Put(funct.lnbeta(t, t), 3, 4, 0);
         Ada.Text_IO.New_Line;
      end loop;
      Ada.Text_IO.Put_Line("  N choose K");
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
         Ada.Text_IO.Put("| ");
         float_io.Put(sum, 4, 0, 0);
         Ada.Text_IO.New_Line;
      end loop;
   end test_func;
--  ----------------------------------------------------------------------------
   --
   --  The following two variables have to be global so that they can be
   --  used by the function integ_f1.  They may be eventually moved into
   --  the private part of the spec, if they need to be used in more places.
   --
pt  : BBS.Numerical.plot.point;
pl  : BBS.Numerical.plot_latex_linear.linear_latex_plot_record;

   function integ_f1(x : real) return real is
   begin
      pt.x := x;
      pt.y := (100.0/(x*x*x))*elem.sin(10.0/((x-0.5)**2));
      pl.draw_glyph(pt, BBS.Numerical.plot.glyph_X, "blue");
      return pt.y;
   end;

   function integ_f2(x : real) return real is
   begin
      return x*2.0 + 1.0;
   end;

   procedure test_integ is
      pList : BBS.Numerical.plot.point_list(0 .. 200);
      y   : real;
      tol : real;
   begin
      Ada.Text_IO.Put_Line("Testing numerical integration routines.");
      Ada.Text_IO.Put_Line("Simple formula 2x+1");
      --
      y := integ.midpoint(integ_f2'Access, 1.0, 3.0, 50);
      Ada.Text_IO.Put("  Midpoint method gives ");
      float_io.Put(y, 2, 3, 0);
      Ada.Text_IO.New_Line;
      --
      y := integ.trapezoid(integ_f2'Access, 1.0, 3.0, 50);
      Ada.Text_IO.Put("  Trapazoid rule gives ");
      float_io.Put(y, 2, 3, 0);
      Ada.Text_IO.New_Line;
      --
      y := integ.simpson(integ_f2'Access, 1.0, 3.0, 50);
      Ada.Text_IO.Put("  Simpson rule gives ");
      float_io.Put(y, 2, 6, 0);
      Ada.Text_IO.New_Line;
      --
      tol := 0.0;
      y := integ.romberg(integ_f2'Access, 1.0, 3.0, tol, 5);
      Ada.Text_IO.Put("  Romberg rule gives ");
      float_io.Put(y, 2, 6, 0);
      Ada.Text_IO.Put(" with estimated tolerance ");
      float_io.Put(tol, 2, 6, 0);
      Ada.Text_IO.New_Line;
      --
      Ada.Text_IO.Put_Line("More complex formula x^3/100*sin(10/((x-0.5)^2))");
      for x in pList'Range loop
         pList(x).x := 1.0 + Float(x)*(3.0 - 1.0)/200.0;
         pList(x).y := integ_f1(pList(x).x);
      end loop;
      pl.start_plot("tadapt-plot.tex", 1.0, 3.0, -100.0, 100.0);
      pl.frame(10, 10, False, False);
      pl.title("$\int_1^3 \frac{100}{x^3}sin(\frac{10}{(x-0.5)^2}) dx$ Using Adaptive Trapezoid Integration");
      pl.draw_line(pList, "red");
      pt := (1.7, -40.0);
      pl.draw_glyph(pt, BBS.Numerical.plot.glyph_X, "blue");
      pt.x := 1.8;
      pl.draw_text(pt, "black", "Function evaluations");
      tol := 1.0e-1;
      y := integ.adapt_trap(integ_f1'Access, 1.0, 3.0, tol, 10);
      pl.end_plot;
      Ada.Text_IO.Put("  Adaptive trapezoid gives ");
      float_io.Put(y, 2, 6, 0);
      Ada.Text_IO.Put(" with estimated tolerance ");
      float_io.Put(tol, 2, 6, 0);
      Ada.Text_IO.New_Line;
      --
      pl.start_plot("integ-plot.tex", 1.0, 3.0, -100.0, 100.0);
      pl.frame(10, 10, False, False);
      pl.title("$\int_1^3 \frac{100}{x^3}sin(\frac{10}{(x-0.5)^2}) dx$ Using Adaptive Simpson's Integration");
      pl.draw_line(pList, "red");
      pt := (1.7, -40.0);
      pl.draw_glyph(pt, BBS.Numerical.plot.glyph_X, "blue");
      pt.x := 1.8;
      pl.draw_text(pt, "black", "Function evaluations");
      tol := 1.0e-1;
      y := integ.adapt_simpson(integ_f1'Access, 1.0, 3.0, tol, 8);
      pl.end_plot;
      Ada.Text_IO.Put("  Adaptive Simpson's gives ");
      float_io.Put(y, 2, 6, 0);
      Ada.Text_IO.Put(" with estimated tolerance ");
      float_io.Put(tol, 2, 6, 0);
      Ada.Text_IO.New_Line;
      --
      tol := 1.0e-2;
      y := integ.romberg(integ_f1'Access, 1.0, 3.0, tol, 10);
      Ada.Text_IO.Put("  Romberg's gives ");
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
      p4 : interp.point;
      p5 : interp.point;
      x  : real;
      y0 : real;
      y1 : real;
      y2 : real;
      y3 : real;
      y4 : real;
   begin
      Ada.Text_IO.Put_Line("Testing interpolation");
      p1.x := 1.0;
      p1.y := interp_f1(p1.x);
      p2.x := 2.0;
      p2.y := interp_f1(p2.x);
      p3.x := 1.3;
      p3.y := interp_f1(p3.x);
      p4.x := 1.7;
      p4.y := interp_f1(p4.x);
      p5.x := 1.5;
      p5.y := interp_f1(p5.x);
      Ada.Text_IO.Put_Line("    N      Actual     2 point    Error      3 point    Error      4 point    Error      5 point    Error");
      for i in 0 .. 30 loop
         x := real(i)*0.1;
         y0 := interp_f1(x);
         y1 := interp.lag2(p1, p2, x);
         y2 := interp.lag3(p1, p2, p3, x);
         y3 := interp.lag4(p1, p2, p3, p4, x);
         y4 := interp.lag5(p1, p2, p3, p4, p5, x);
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
         Ada.Text_IO.Put("  ");
         float_io.Put(y3, 2, 6, 0);
         Ada.Text_IO.Put("  ");
         float_io.Put(y0 - y3, 2, 6, 0);
         Ada.Text_IO.Put("  ");
         float_io.Put(y4, 2, 6, 0);
         Ada.Text_IO.Put("  ");
         float_io.Put(y0 - y4, 2, 6, 0);
         Ada.Text_IO.New_Line;
      end loop;
   end;
--  ----------------------------------------------------------------------------
   --  First example differential equation
   --
   --  dy
   --  --  = -y + t + 1
   --  dt
   function ode_f1(t, y : Real) return Real is
   begin
      return (-y + t + 1.0);
   end;
   --
   --  If y(0) = 1, then y(t) = t + exp(-t)
   --
   function ode_f1_exact(t : Real) return Real is
   begin
      return (t + elem.Exp(-t));
   end;
   --  Second example differential equation
   --  dy     y^2
   --  -- = - ---
   --  dt     t^2
   function ode_f2(t, y : Real) return Real is
   begin
      return -(y*y)/(t*t);
   end;
   --
   --  If y(0) = C, then y(t) = t/(Ct - 1)
   --
   function ode_f2_exact(t : Real) return Real is
      c : constant Real := 2.0;
   begin
      return t/(c*t - 1.0);
   end;
   --
   --  Differential equation system
   --
   function ode_f1sys(t : Real; y : ode.params) return Real is
   begin
      return -4.0*y(1) + 3.0*y(2) + 6.0;
   end;

   function ode_f2sys(t : Real; y : ode.params) return Real is
   begin
      return -2.4*y(1) + 1.6*y(2) + 3.6;
   end;
   --
   --  Second order equation
   --
   function ode2_f1(t, y : Real) return Real is
   begin
      return -9.8;
   end;
   --
   procedure test_ode is
      type param_array is array(1 .. 2) of real;
      type sys_func is access function (t : real; y : ode.params) return real;
      pl   : BBS.Numerical.plot_latex_linear.linear_latex_plot_record;
      ps   : BBS.Numerical.plot_svg_linear.linear_svg_plot_record;
      p    : BBS.Numerical.plot.point_err;
      line : BBS.Numerical.plot.point_list(0 .. 200);
      t    : Real;
      t0   : constant Real := 1.0;  --  Starting time
      y0   : Real := ode_f2_exact(t0);  --  Initial value
      yr   : Real := y0;    -- 4th order Runge-Kutta
      yrkf : Real := y0;    -- Runge-Kutta-Fehlberg
      ye   : Real := y0;    -- Euler
      ypc  : Real;          --  Adams-Bashforth-Moulton predictor/corrector
      tol  : Real;
      y1   : Real;
      y2   : Real;
      y3   : Real;
      yst0 : Real;
      yst1 : Real;
      step : constant real := 0.05;
      ysys : ode.params(1..2);
      rsys : ode.params(1..2);
      func : constant ode.functs(1 .. 2) := (ode_f1sys'Access, ode_f2sys'Access);
   begin
      for i in 0 .. 200 loop
         t := Real(i)*step*0.1 + t0;
         line(i).x := t;
         line(i).y := ode_f2_exact(t);
      end loop;
      pl.start_plot("ode-plot.tex", t0, t0 + 1.0, 0.5, 1.0);
      pl.frame(10, 10, False, False);
      pl.title("Runge-Kutta-Fehlberg");
      pl.draw_line(line, "red");
      ps.start_plot("ode-plot.svg", t0, t0 + 1.0, 0.5, 1.0);
      ps.frame(10, 10, False, False);
      ps.title("Runge-Kutta-Fehlberg");
      ps.draw_line(line, "blue");
      --
      Ada.Text_IO.Put_Line("Testing differential equations.");
      Ada.Text_IO.Put_Line("   Time     Exact     Euler's     RK4        RKF        RKF Tol         ABAM");
      t    := t0;
      Ada.Text_IO.Put("  ");
      float_io.Put(t, 2, 2, 0);
      Ada.Text_IO.Put("  ");
      float_io.Put(ode_f2_exact(t), 2, 6, 0);
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
         t := t + step;
         p.x := t;
         p.y := yrkf;
         p.e := tol;
         Ada.Text_IO.Put("  ");
         float_io.Put(t, 2, 2, 0);
         Ada.Text_IO.Put("  ");
         float_io.Put(ode_f2_exact(t), 2, 6, 0);
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
         pl.draw_glyph(p, BBS.Numerical.plot.glyph_X, "red");
         ps.draw_glyph(p, BBS.Numerical.plot.glyph_X, "red");
      end loop;
      pl.end_plot;
      ps.end_plot;
      --
      Ada.Text_IO.Put_Line("Testing Differential Equation Systems");
      Ada.Text_IO.Put_Line("   Time   RK4-a      RK4-b");
      ysys := (0.0, 0.0);
      t := 0.0;
      Ada.Text_IO.Put("  ");
      float_io.Put(0.0, 2, 2, 0);
      Ada.Text_IO.Put("  ");
      float_io.Put(ysys(1), 2, 6, 0);
      Ada.Text_IO.Put("  ");
      float_io.Put(ysys(2), 2, 6, 0);
      Ada.Text_IO.New_Line;
      for i in 1 .. 20 loop
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
      --
      Ada.Text_IO.Put_Line("Testing 2nd Order Stormer");
      Ada.Text_IO.Put_Line("   Time   Position");
      t := 0.0;
      yst0 := 0.0;
      yst1 := 0.0;
      Ada.Text_IO.Put("  ");
      float_io.Put(t, 2, 2, 0);
      Ada.Text_IO.Put("  ");
      float_io.Put(yst0, 2, 6, 0);
      Ada.Text_IO.New_Line;
      t := step;
      yst0 := ode.stormer_start(ode2_f1'Access, t, yst0, 10.0, step);
      Ada.Text_IO.Put("  ");
      float_io.Put(t, 2, 2, 0);
      Ada.Text_IO.Put("  ");
      float_io.Put(yst0, 2, 6, 0);
      Ada.Text_IO.Put("  ");
      float_io.Put((yst0 - yst1)/step, 2, 6, 0);
      Ada.Text_IO.New_Line;
      t := t + step;
      yst1 := yst0;
      for i in 2 .. 20 loop
         yst0 := ode.stormer_next(ode2_f1'Access, t, yst0, yst1, step);
         Ada.Text_IO.Put("  ");
         float_io.Put(t, 2, 2, 0);
         Ada.Text_IO.Put("  ");
         float_io.Put(yst0, 2, 6, 0);
         Ada.Text_IO.Put("  ");
         float_io.Put((yst0 - yst1)/step, 2, 6, 0);
         Ada.Text_IO.New_Line;
         t := t + step;
         yst1 := yst0;
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
   procedure test_random is
      lin   : BBS.Numerical.Random.LCG;
      twist : BBS.Numerical.Random.MT;
   begin
      Ada.Text_IO.Put_Line("Testing Random Numbers.");
      Ada.Text_IO.Put_Line("   LCG        MT");
      lin.init;
      twist.init;
      for i in 0 .. 20 loop
         uint32_io.Put(lin.getNext, 10);
         Ada.Text_IO.Put("  ");
         float_io.Put(real(lin.getNextF), 1, 6, 0);
         Ada.Text_IO.Put("  ");
         uint32_io.Put(twist.getNext, 10);
         Ada.Text_IO.Put("  ");
         float_io.Put(real(twist.getNextF), 1, 6, 0);
         Ada.Text_IO.New_Line;
      end loop;
   end;
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
      pl  : BBS.Numerical.plot_latex_linear.linear_latex_plot_record;
      ps  : BBS.Numerical.plot_svg_linear.linear_svg_plot_record;
      p   : BBS.Numerical.plot.point;
      val : real;
      dof : Positive := 10;
   begin
      Ada.Text_IO.Put_Line("Testing statistical routines.");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line("Probability Distributions");
      Ada.Text_IO.Put_Line("               Normal                     Chi^2                     Student's T           Poisson             Exponential");
      Ada.Text_IO.Put_Line("   X     PDF           CDF           PDF           CDF           PDF           CDF           PMF           PDF           CDF");
      pl.start_plot("stat-plot.tex", 0.0, 20.0, 0.0, 1.0);
      pl.frame(10, 10, False, False);
      pl.label("x-axis", "Probability density");
      pl.title("Probability densities (DOF=" & Positive'Image(dof) & ")");
      ps.start_plot("stat-plot.svg", 0.0, 20.0, 0.0, 1.0);
      ps.frame(10, 10, False, False);
      ps.label("x-axis", "Probability density");
      ps.title("Probability densities (DOF=" & Positive'Image(dof) & ")");
      for i in 0 .. 20 loop
         val := real(i)*1.0;
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
         float_io.Put(stat.exp_cdf(val, 1.0), 1, 6, 3);
         Ada.Text_IO.Put("  ");
         float_io.Put(stat.exp_pdf(val, 1.0), 1, 6, 3);
         Ada.Text_IO.Put("  ");
         float_io.Put(stat.poisson_pmf(Natural(i), dof), 1, 6, 3);
         Ada.Text_IO.New_Line;
         p.x := val;
         p.y := stat.normal_pdf(val);
         pl.draw_glyph(p, BBS.Numerical.plot.glyph_plus, "red");
         ps.draw_glyph(p, BBS.Numerical.plot.glyph_plus, "red");
         p.y := stat.normal_cdf(0.0, val, 20);
         pl.draw_glyph(p, BBS.Numerical.plot.glyph_diamond, "red");
         ps.draw_glyph(p, BBS.Numerical.plot.glyph_diamond, "red");
         p.y := stat.chi2_pdf(val, dof);
         pl.draw_glyph(p, BBS.Numerical.plot.glyph_X, "blue");
         ps.draw_glyph(p, BBS.Numerical.plot.glyph_X, "blue");
         p.y := stat.chi2_cdf(0.0, val, dof, 20);
         pl.draw_glyph(p, BBS.Numerical.plot.glyph_box, "blue");
         ps.draw_glyph(p, BBS.Numerical.plot.glyph_box, "blue");
         p.y := stat.studentT_pdf(val, dof);
         pl.draw_glyph(p, BBS.Numerical.plot.glyph_asterisk, "green");
         ps.draw_glyph(p, BBS.Numerical.plot.glyph_asterisk, "green");
         p.y := stat.studentT_cdf(0.0, val, dof, 20);
         pl.draw_glyph(p, BBS.Numerical.plot.glyph_octagon, "green");
         ps.draw_glyph(p, BBS.Numerical.plot.glyph_octagon, "green");
         p.y := stat.exp_pdf(val, 1.0);
         pl.draw_point(p, 0.05, "cyan");
         ps.draw_point(p, 1.0, "cyan");
         p.y := stat.exp_cdf(val, 1.0);
         pl.draw_point(p, 0.1, "cyan");
         ps.draw_point(p, 2.0, "cyan");
         p.y := stat.poisson_pmf(Natural(i), dof);
         pl.draw_point(p, 0.05, "black");
         ps.draw_point(p, 1.0, "black");
      end loop;
      p := (4.0, 0.9);
      pl.draw_glyph(p, BBS.Numerical.plot.glyph_plus, "red");
      ps.draw_glyph(p, BBS.Numerical.plot.glyph_plus, "red");
      p.x := p.x + 0.5;
      pl.draw_text(p, "black", "Normal PDF");
      ps.draw_text(p, "black", "Normal PDF");
      p := (4.0, 0.85);
      pl.draw_glyph(p, BBS.Numerical.plot.glyph_diamond, "red");
      ps.draw_glyph(p, BBS.Numerical.plot.glyph_diamond, "red");
      p.x := p.x + 0.5;
      pl.draw_text(p, "black", "Normal CDF");
      ps.draw_text(p, "black", "Normal CDF");
      p := (4.0, 0.8);
      pl.draw_glyph(p, BBS.Numerical.plot.glyph_X, "blue");
      ps.draw_glyph(p, BBS.Numerical.plot.glyph_X, "blue");
      p.x := p.x + 0.5;
      pl.draw_text(p, "black", "$\chi^2$ PDF");
      ps.draw_text(p, "black", "$\chi^2$ PDF");
      p := (4.0, 0.75);
      pl.draw_glyph(p, BBS.Numerical.plot.glyph_box, "blue");
      ps.draw_glyph(p, BBS.Numerical.plot.glyph_box, "blue");
      p.x := p.x + 0.5;
      pl.draw_text(p, "black", "$\chi^2$ CDF");
      ps.draw_text(p, "black", "$\chi^2$ CDF");
      p := (4.0, 0.7);
      pl.draw_point(p, 0.05, "black");
      ps.draw_point(p, 1.0, "black");
      p.x := p.x + 0.5;
      pl.draw_text((4.5, 0.7), "black", "Poisson PMF");
      ps.draw_text((4.5, 0.7), "black", "Poisson PMF");
      p := (4.0, 0.65);
      pl.draw_glyph(p, BBS.Numerical.plot.glyph_asterisk, "green");
      ps.draw_glyph(p, BBS.Numerical.plot.glyph_asterisk, "green");
      p.x := p.x + 0.5;
      pl.draw_text(p, "black", "Student's T PDF");
      ps.draw_text(p, "black", "Student's T PDF");
      p := (4.0, 0.6);
      pl.draw_glyph(p, BBS.Numerical.plot.glyph_octagon, "green");
      ps.draw_glyph(p, BBS.Numerical.plot.glyph_octagon, "green");
      p.x := p.x + 0.5;
      pl.draw_text(p, "black", "Student's T CDF");
      ps.draw_text(p, "black", "Student's T CDF");
      p := (10.0, 0.9);
      pl.draw_point(p, 0.05, "cyan");
      ps.draw_point(p, 1.0, "cyan");
      p.x := p.x + 0.5;
      pl.draw_text(p, "black", "Exponential PDF");
      ps.draw_text(p, "black", "Exponential PDF");
      p := (10.0, 0.85);
      pl.draw_point(p, 0.1, "cyan");
      ps.draw_point(p, 2.0, "cyan");
      p.x := p.x + 0.5;
      pl.draw_text(p, "black", "Exponential CDF");
      ps.draw_text(p, "black", "Exponential CDF");
      pl.end_plot;
      ps.end_plot;
      Ada.Text_IO.New_Line;
   end test_stats;
--  ----------------------------------------------------------------------------
   procedure test_regression is
      data : linreg.data_array :=
         ((x => 1.0, y => 1.0),
         (x => 2.0, y => 1.0),
         (x => 3.0, y => 2.0),
         (x => 4.0, y => 2.0),
         (x => 5.0, y => 4.0));
      res  : linreg.simple_linreg_result;
   begin
      Ada.Text_IO.Put_Line("Testing regression");
      res := linreg.simple_linear(data);
      Ada.Text_IO.Put_Line("Linear regression result:");
      Ada.Text_IO.Put("  a = ");
      float_io.Put(res.a, 2, 3, 0);
      Ada.Text_IO.Put(", b = ");
      float_io.Put(res.b, 2, 3, 0);
      Ada.Text_IO.Put(", SSe = ");
      float_io.Put(res.SSe, 2, 3, 0);
      Ada.Text_IO.Put(", variance = ");
      float_io.Put(res.var, 2, 3, 0);
      Ada.Text_IO.Put(", slope variance = ");
      float_io.Put(res.Bvar, 2, 3, 0);
      Ada.Text_IO.Put(", correlation coefficient (R) = ");
      float_io.Put(res.cor, 2, 3, 0);
      Ada.Text_IO.New_Line;
   end;
--  ----------------------------------------------------------------------------
   procedure test_plot is
      pl : BBS.Numerical.plot_latex_linear.linear_latex_plot_record;
      p1 : BBS.Numerical.plot.point_list(0 .. 20);
      p2 : BBS.Numerical.plot.point_list(0 .. 20);
   begin
      for x in 0 .. 20 loop
         p1(x).x := Float(x) - 10.0;
         p1(x).y := 5.0*elem.sin(Float(x)/3.0);
         p2(x).x := 5.0*elem.sin(Float(x));
         p2(x).y := 5.0*elem.cos(Float(x));
      end loop;
      pl.start_plot("test-plot.tex", -10.0, 10.0, -10.0, 10.0);
      pl.frame(10, 10, False, False);
      pl.label("x-Axis", "y-Axis");
      pl.title("The Title of the Plot");
      pl.draw_line(p1, "red");
      pl.draw_glyph(p1, BBS.Numerical.plot.glyph_diamond, "red");
      pl.draw_point(p2, 0.1, "green");
      pl.end_plot;
   end;
--  ----------------------------------------------------------------------------
   procedure test_filter is
      pl : BBS.Numerical.plot_latex_linear.linear_latex_plot_record;
      ps : BBS.Numerical.plot_svg_linear.linear_svg_plot_record;
      p1 : BBS.Numerical.plot.point_list(0 .. 100);
      p2 : BBS.Numerical.plot.point_list(0 .. 100);
      f1 : filter.average(2);
      pt : BBS.Numerical.plot.point;
   begin
      for x in 0 .. 100 loop
         p1(x).x := Float(x);
         p1(x).y := elem.sin(Float(x)/10.1) + elem.cos(Float(x)*2.1);
         p2(x).x := Float(x);
         p2(x).y := f1.filter(p1(x).y);
      end loop;
      pl.start_plot("filter-plot.tex", 0.0, 100.0, -2.0, 2.0);
      pl.frame(10, 10, False, False);
      pl.label("Time", "Data");
      pl.Title("Data Filtering with an Averaging Filter");
      pl.draw_glyph(p1, BBS.Numerical.plot.glyph_X, "red");
      pl.draw_line(p1, "red");
      pl.draw_glyph(p2, BBS.Numerical.plot.glyph_plus, "blue");
      pl.draw_line(p2, "blue");
      pt := (x => 5.0, y => -0.8);
      pl.draw_glyph(pt, BBS.Numerical.plot.glyph_x, "red");
      pt := (x => 5.6, y => -0.8);
      pl.draw_text(pt, "black", "Original Data");
      pt := (x => 5.0, y => -1.0);
      pl.draw_glyph(pt, BBS.Numerical.plot.glyph_plus, "blue");
      pt := (x => 5.6, y => -1.0);
      pl.draw_text(pt, "black", "Filtered Data");
      pl.end_plot;
      --
      ps.start_plot("filter-plot.svg", 0.0, 100.0, -2.0, 2.0);
      ps.frame(10, 10, False, False);
      ps.label("Time", "Data");
      ps.Title("Data Filtering with an Averaging Filter");
      ps.draw_glyph(p1, BBS.Numerical.plot.glyph_X, "red");
      ps.draw_line(p1, "red");
      ps.draw_glyph(p2, BBS.Numerical.plot.glyph_plus, "blue");
      ps.draw_line(p2, "blue");
      pt := (x => 5.0, y => -0.8);
      ps.draw_glyph(pt, BBS.Numerical.plot.glyph_x, "red");
      pt := (x => 5.6, y => -0.8);
      ps.draw_text(pt, "black", "Original Data");
      pt := (x => 5.0, y => -1.0);
      ps.draw_glyph(pt, BBS.Numerical.plot.glyph_plus, "blue");
      pt := (x => 5.6, y => -1.0);
      ps.draw_text(pt, "black", "Filtered Data");
      ps.end_plot;
   end;
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
