--
--  This is a simple package that supports basic functions on quaternions.
--
--  i*i = j*j = k*k = i*j*k = -1
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
   function "*" (Left : quaternion; Right : quaternion) return quaternion;

   function "/" (Left : quaternion; Right : f) return quaternion;

   function magnitude(self : in quaternion) return f'Base;
   --
   --  Scale value to have a magnitude of 1.  This is effectively a
   --  unit vector.
   --
   function normalize(self : in quaternion) return quaternion;

end BBS.Numerical.quaternion;
