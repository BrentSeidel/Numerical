with Ada.Numerics.Generic_Elementary_Functions;
package body BBS.Numerical.quaternion is
   package elem is new Ada.Numerics.Generic_Elementary_Functions(f);
   --
   --  Basic operations
   --
   function "+" (Left, Right : quaternion) return quaternion is
   begin
      return (r => Left.r + Right.r,
              i => Left.i + Right.i,
              j => Left.j + Right.j,
              k => Left.k + Right.k);
   end;
   --
   function "-" (Left, Right : quaternion) return quaternion is
   begin
      return (r => Left.r - Right.r,
              i => Left.i - Right.i,
              j => Left.j - Right.j,
              k => Left.k - Right.k);
   end;

   function "*" (Left : f; Right : quaternion) return quaternion is
   begin
      return (r => Left * Right.r,
              i => Left * Right.i,
              j => Left * Right.j,
              k => Left * Right.k);
   end;
   --
   function "*" (Left : quaternion; Right : f) return quaternion is
   begin
      return (r => Left.r * Right,
              i => Left.i * Right,
              j => Left.j * Right,
              k => Left.k * Right);
   end;
   --
   function "*" (Left : quaternion; Right : quaternion) return quaternion is
   begin
      return (r => Left.r*Right.r - Left.i*Right.i - Left.j*Right.j - Left.k*Right.k,
              i => Left.r*Right.i + Left.i*Right.r + Left.j*Right.k - Left.k*Right.j,
              j => Left.r*Right.j - Left.i*Right.k + Left.j*Right.r + Left.k*Right.i,
              k => Left.r*Right.k + Left.i*Right.j - Left.j*Right.i + Left.k*Right.r);
   end;
   --
   function "/" (Left : quaternion; Right : f) return quaternion is
   begin
      return (r => Left.r / Right,
              i => Left.i / Right,
              j => Left.j / Right,
              k => Left.k / Right);
   end;
   --
   function magnitude(self : in quaternion) return f'Base is
   begin
      return elem.Sqrt(self.r * self.r + self.i * self.i +
                       self.j * self.j + self.k * self.k);
   end;
   --
   --  Scale value to have a magnitude of 1.  This is effectively a
   --  unit vector.
   --
   function normalize(self : in quaternion) return quaternion is
      m : constant f'Base := self.magnitude;
   begin
      return (r => self.r/m, i => self.i/m, j => self.j/m, k => self.k/m);
   end;
end BBS.Numerical.quaternion;
