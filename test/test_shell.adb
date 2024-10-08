with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with test_cases;

procedure test_shell is
   selection : Integer := 0;
begin
   Ada.Text_IO.Put_Line("Numerical analysis routine tests.");
   loop
      Ada.Text_IO.Put_Line("Tests available are:");
      Ada.Text_IO.Put_Line("  1 - Numerical derivatives");
      Ada.Text_IO.Put_Line("  2 - Common functions");
      Ada.Text_IO.Put_Line("  3 - Interpolation");
      Ada.Text_IO.Put_Line("  4 - Numerical integration");
      Ada.Text_IO.Put_Line("  5 - Differential Equations");
      Ada.Text_IO.Put_Line("  6 - Complex polynomials");
      Ada.Text_IO.Put_Line("  7 - Real polynomials");
      Ada.Text_IO.Put_Line("  8 - Root finding");
      Ada.Text_IO.Put_Line("  9 - Statistics (and plot)");
      Ada.Text_IO.Put_Line(" 10 - Random numbers");
      Ada.Text_IO.Put_Line(" 11 - Regression");
      Ada.Text_IO.Put_Line(" 12 - Filtering (and plot)");
      Ada.Text_IO.Put("Selection: ");
      Ada.Integer_Text_IO.Get(selection, 0);
      --
      --  This is just to clear out any text on the rest of the line.
      --
      declare
         dummy : String := Ada.Text_IO.Get_line;
      begin
         null;  --  Nothing to do here.
      end;
      exit when (selection > 0) and (selection < 14);
   end loop;
   if selection = 1 then
      test_cases.test_derive;
   elsif selection = 2 then
      test_cases.test_func;
   elsif selection = 3 then
      test_cases.test_interp;
   elsif selection = 4 then
      test_cases.test_integ;
   elsif selection = 5 then
      test_cases.test_ode;
   elsif selection = 6 then
      test_cases.test_poly_cmplx;
   elsif selection = 7 then
      test_cases.test_poly;
   elsif selection = 8 then
      test_cases.test_root;
   elsif selection = 9 then
      test_cases.test_stats;
   elsif selection = 10 then
      test_cases.test_random;
   elsif selection = 11 then
      test_cases.test_regression;
   elsif selection = 12 then
      test_cases.test_filter;
   end if;
end;
