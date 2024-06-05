with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Text_IO;
with BBS.Numerical;
with BBS.Numerical.regression;
with BBS.Numerical.Statistics;
with BBS.Numerical.functions_real;

procedure test_stats is
   subtype real is Float;
   package linreg is new BBS.Numerical.regression(real);
   package stat is new BBS.Numerical.Statistics(real);
   package float_io is new Ada.Text_IO.Float_IO(real);
   package elem is new Ada.Numerics.Generic_Elementary_Functions(real);
   package funct is new BBS.Numerical.functions_real(real);

   data : linreg.data_array :=
      ((x => 1.0, y => 1.0),
       (x => 2.0, y => 1.0),
       (x => 3.0, y => 2.0),
       (x => 4.0, y => 2.0),
       (x => 5.0, y => 4.0));
   res  : linreg.simple_linreg_result;
   val : real;
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
   Ada.Text_IO.Put_Line("             Normal             Chi^2");
   Ada.Text_IO.Put_Line("     X     PDF      CDF      PDF      PDF      CDF");
   for i in 0 .. 20 loop
      val := real(i)*1.0;
      Ada.Text_IO.Put("  ");
      float_io.Put(val, 2, 2, 0);
      Ada.Text_IO.Put("  ");
      float_io.Put(stat.normal_pdf(val), 1, 5, 0);
      Ada.Text_IO.Put("  ");
      float_io.Put(stat.normal_cdf(0.0, val, 20), 1, 5, 0);
      Ada.Text_IO.Put("  ");
      float_io.Put(stat.chi2_pdf(val, 20), 1, 5, 0);
      Ada.Text_IO.Put("  ");
      float_io.Put(stat.chi2_exp(val, 20), 1, 5, 0);
      Ada.Text_IO.Put("  ");
      float_io.Put(stat.chi2_cdf(0.0, val, 20, 20), 1, 5, 0);
      Ada.Text_IO.New_Line;
   end loop;
end test_stats;
