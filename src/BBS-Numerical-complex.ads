--
--  There already is a complex number package as part of Ada.Numerics.  So, this
--  package isn't really needed.  This is more of a practice run for me.
--
--  In general, the only time you would want to use this package is if the
--  Ada.Numerics.Generic_Complex_Types package is unavailable for some
--  reason or another.
--
--  The one difference is that in this package, complex numbers are an object
--  so that object notation can be used in many case.
--
generic
  type F is digits <>;
package BBS.Numerical.complex is
   type complex is tagged record
      r : f'Base;
      i : f'Base;
   end record;
   --
   --  Basic manipulations
   --
   function real(self : in complex) return f'Base;
   function imaginary(self : in complex) return f'Base;
   procedure set_cartisian(self : out complex; real, imag : f);
   function conjugate(self : in complex) return complex;
   --
   --  Basic arithmatic operations
   --
   function "+" (Left, Right : complex) return complex;
   function "-" (Left, Right : complex) return complex;
   function "*" (Left, Right : complex) return complex;
   function "*" (Left : f; Right : complex) return complex;
   function "*" (Left : complex; Right : f) return complex;
   function "/" (Left, Right : complex) return complex;
   function "/" (Left : complex; Right : f) return complex;
   --
   --  Polar related
   --
   procedure set_polar(self : out complex; mag, angle : f);
   function magnitude(self : in complex) return f'Base;
   function angle(self : in complex) return f'Base;
   --
   --  Comparisons
   --  Note that complex numbers are not ordered so there is no comparison to
   --  one being greater or less than another.  These functions test for equality
   --  within a tolerence.
   --
   --  Test if value's real and imaginary parts are with in the tolerence of the
   --  test.  This is effectively if value is within a square centered on self.
   --
   function near_sq(self : in complex; value : complex; tol : f) return Boolean;
   --
   --  Test if value's distance from self is with in the tolerence.  This is
   --  effectively if value is within a circle centered on self.
   --
   function near_cir(self : in complex; value : complex; tol : f) return Boolean;
   --
   --  Additional functions
   --
   function exp(self : in complex) return complex;
   --
   --  Utility print procedure for debugging
   --
   procedure print(self : in complex; fore, aft, exp : Natural);
   --
   --  Constants
   --
   zero : constant complex := (r => 0.0, i => 0.0);
   one : constant complex := (r => 1.0, i => 0.0);
   j : constant complex := (r => 0.0, i => 1.0);
   i : complex renames j;
private

end BBS.Numerical.complex;
