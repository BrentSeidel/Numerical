generic
  type F is digits <>;
package BBS.Numerical.vector is
   type vect is array (integer range <>) of f'Base;
   --
   --  Basic arithmatic operations
   --
   function "+" (Left, Right : vect) return vect
      with pre => (Left'First = Right'First) and (Left'Last = Right'Last);
   function "-" (Left, Right : vect) return vect
      with pre => (Left'First = Right'First) and (Left'Last = Right'Last);
   --
   --  Vector dot product
   --
   function "*" (Left, Right : vect) return f'Base
      with pre => (Left'First = Right'First) and (Left'Last = Right'Last);
   function "*" (Left : f; Right : vect) return vect;
   function "*" (Left : vect; Right : f) return vect;
   --

   function magnitude(self : in vect) return f'Base;
   --
   --  Scale value to have a magnitude of 1.  This is effectively a
   --  unit vector.
   --
   function normalize(self : in vect) return vect;
   --
end BBS.Numerical.vector;
