with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Text_IO;
with BBS.Numerical;
with BBS.Numerical.functions_real;

procedure test_func is
   subtype real is Float;
   package funct is new BBS.Numerical.functions_real(real);
   package float_io is new Ada.Text_IO.Float_IO(real);
   package elem is new Ada.Numerics.Generic_Elementary_Functions(real);

begin
   Ada.Text_IO.Put_Line("Testing some of the numerical routines.");
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
