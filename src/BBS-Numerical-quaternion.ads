--
--  This is a simple package that supports basic functions on quaternions.
--
generic
  type F is digits <>;
package BBS.Numerical.quaternion is
   type quaternion is tagged record
      r : f'Base;  --  Scalar part
      i : f'Base;  --  i, j, and k are the vector part
      j : f'Base;
      k : f'Base;
   end record;
   --
   --  Basic arithmatic operations
   --
   function "+" (Left, Right : quaternion) return quaternion;
   function "-" (Left, Right : quaternion) return quaternion;

   function "*" (Left : f; Right : quaternion) return quaternion;
   function "*" (Left : quaternion; Right : f) return quaternion;

   function "/" (Left : quaternion; Right : f) return quaternion;

   function magnitude(self : in quaternion) return f'Base;

end BBS.Numerical.quaternion;
