with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Text_IO;
with BBS.Numerical;
with BBS.Numerical.roots_real;

procedure test_root is
   subtype real is Long_Float;
   package root is new BBS.Numerical.roots_real(real);
   package float_io is new Ada.Text_IO.Float_IO(real);
   package elem is new Ada.Numerics.Generic_Elementary_Functions(real);

   function f1(x : real) return real is
   begin
      return (100.0/(x*x))*elem.sin(10.0/x);
   end;

   function f2(x : real) return real is
   begin
      return x*x*x + 4.0*x*x - 10.0;
   end;

   r   : real;
   l   : real;
   u   : real;
   err : root.errors;
begin
   Ada.Text_IO.Put_Line("Testing some of the numerical routines.");
   --
   Ada.Text_IO.Put_Line("Testing root finding:");
   l := 1.0;
   u := 2.0;
   r := root.bisection(f2'Access, l, u, 13, err);
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
   r := root.seacant(f2'Access, l, u, 13, err);
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
   r := root.mueller(f2'Access, l, u, 13, err);
   Ada.Text_IO.Put("  Mueller gives root at");
   float_io.Put(r, 2, 9, 0);
   Ada.Text_IO.Put(", in range ");
   float_io.Put(l, 2, 6, 0);
   Ada.Text_IO.Put(" to ");
   float_io.Put(u, 2, 6, 0);
   Ada.Text_IO.Put_Line(", with error code " & root.errors'Image(err));
end test_root;
