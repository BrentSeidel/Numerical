with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Text_IO;
package body BBS.Numerical.complex_real is
   package elem is new Ada.Numerics.Generic_Elementary_Functions(f'Base);
   package float_io is new Ada.Text_IO.Float_IO(f'Base);
   --
   --  Basic manipulations
   --
   function real(self : in complex) return f'Base is
   begin
      return self.r;
   end;
   --
   function imaginary(self : in complex) return f'Base is
   begin
      return self.i;
   end;
   --
   procedure set_cartisian(self : out complex; real, imag : f) is
   begin
      self.r := real;
      self.i := imag;
   end;
   --
   function conjugate(self : in complex) return complex is
   begin
      return (r => self.r, i => -(self.i));
   end;
   --
   --  Basic operations
   --
   function "+" (Left, Right : complex) return complex is
   begin
      return (r => (Left.r + Right.r),
              i => (Left.i + Right.i));
   end;
   --
   function "-" (Left, Right : complex) return complex is
   begin
      return (r => (Left.r - Right.r),
              i => (Left.i - Right.i));
   end;
   --
   --  This is the standard process for multiplying complext numbers.  There is
   --  a method that only uses three multiplications, but four add/subtract.
   --  For modern FPUs, there is probably no advantage, but if you are using a
   --  software FPU, and a lot of complex multiplies, it might be worthwhile to
   --  look it up and use it here.
   --
   function "*" (Left, Right : complex) return complex is
   begin
      return (r => (Left.r * Right.r) - (Left.i * Right.i),
              i => (Left.r * Right.i) + (Left.i * Right.r));
   end;
   --
   function "*" (Left : f; Right : complex) return complex is
   begin
      return (r => Right.r * Left, i => Right.i * Left);
   end;
   --
   function "*" (Left : complex; Right : f) return complex is
   begin
      return (r => Left.r * Right, i => Left.i * Right);
   end;
   --
   function "/" (Left, Right : complex) return complex is
      conjsq : f := (Right.r * Right.r) + (Right.i * Right.i);
   begin
      return (r => ((Left.r * Right.r) + (Left.i * Right.i))/conjsq,
              i => ((Left.r * Right.i) - (Left.i * Right.r))/conjsq);
   end;
   --
   function "/" (Left : complex; Right : f) return complex is
   begin
      return (r => Left.r / Right, i => Left.i / Right);
   end;
   --
   --  Polar related
   --
   procedure set_polar(self : out complex; mag, angle : f) is
   begin
      self.r := mag * elem.Cos(angle);
      self.i := mag * elem.Sin(angle);
   end;
   --
   function magnitude(self : in complex) return f'Base is
   begin
      return elem.Sqrt((self.r * self.r) + (self.i * self.i));
   end;
   --
   function angle(self : in complex) return f'Base is
   begin
      return elem.Arctan(self.i, self.r);
   end;
   --
   --  Comparisons
   --  Note that complex numbers are not ordered so there is no comparison to
   --  one being greater or less than another.  These functions test for equality
   --  within a tolerence.
   --
   --  Test if value's real and imaginary parts are with in the tolerence of the
   --  test.  This is effectively if value is within a square centered on self.
   --
   function near_sq(self : in complex; value : complex; tol : f) return Boolean is
   begin
      return (abs(self.r - value.r) < tol) and (abs(self.i - value.i) < tol);
   end;
   --
   --  Test if value's distance from self is with in the tolerence.  This is
   --  effectively if value is within a circle centered on self.
   --
   function near_cir(self : in complex; value : complex; tol : f) return Boolean is
      dr : f'Base := self.r - value.r;
      di : f'Base := self.i - value.i;
   begin
      return (dr*dr + di*dr) < tol;
   end;
   --
   --  Additional functions
   --
   function exp(self : in complex) return complex is
      t1 : f := elem.Exp(self.r);
   begin
      return (r => t1 * elem.Cos(self.i), i => t1 * elem.Sin(self.i));
   end;
   --
   --  Utility print procedure for debugging
   --
   procedure print(self : in complex; fore, aft, exp : Natural) is
   begin
      Ada.Text_IO.Put("(");
      float_io.Put(self.r, fore, aft, exp);
      Ada.Text_IO.Put(", ");
      float_io.Put(self.i, fore, aft, exp);
      Ada.Text_IO.Put(")");
   end;
   --
end BBS.Numerical.complex_real;
