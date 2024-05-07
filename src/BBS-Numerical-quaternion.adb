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
end BBS.Numerical.quaternion;
