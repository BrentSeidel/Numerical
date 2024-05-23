with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Text_IO;
with BBS.Numerical;
with BBS.Numerical.derivative_real;

procedure test_derive is
   subtype real is Float;
   package derive is new BBS.Numerical.derivative_real(real);
   package float_io is new Ada.Text_IO.Float_IO(real);
   package elem is new Ada.Numerics.Generic_Elementary_Functions(real);

   function f1(x : real) return real is
   begin
      return elem.log(x);
   end;

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
   Ada.Text_IO.Put_Line("Testing some of the numerical routines.");
   --
   Ada.Text_IO.Put_Line("  Various derivative functions");
   Ada.Text_IO.Put_Line("  h       pt2         pt3a        pt3b        pt5a        pt5b");
   print(f1'Access, 1.8, 0.1);
   print(f1'Access, 1.8, 0.01);
   print(f1'Access, 1.8, 0.001);
   --
   --  Interestingly for this particular function accuracy using Float
   --  is worse with a step size of 0.0001 for most derivative methods.
   --
   print(f1'Access, 1.8, 0.0001);
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put("  Actual value is: ");
   float_io.Put(1.0/1.8, 1, 8, 0);
   Ada.Text_IO.New_Line;
end test_derive;
