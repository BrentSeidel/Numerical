generic
   type F is digits <>;
package BBS.Numerical.interpolation is

   type point is record
      x : f'Base;
      y : f'Base;
   end record;
   type points is array (Integer range <>) of point;
   --
   --  Note interpolations will be most accurate for x between the points.
   --  Outside of that is extrapolation.
   --
   --  Linear interpolation.  This uses a two point Lagrange polynomial
   --
   function linear(p0, p1 : point; x : f'Base) return f'Base;
   --
   --  Quadratic interpolation.  This uses a three point Lagrange polynomial
   --
   function quadratic(p0, p1, p2 : point; x : f'Base) return f'Base;
end;
