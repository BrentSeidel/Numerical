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
   --  Linear interpolation using a two point Lagrange polynomial
   --
   function lag2(p0, p1 : point; x : f'Base) return f'Base;
   --
   --  Quadratic interpolation using a three point Lagrange polynomial
   --
   function lag3(p0, p1, p2 : point; x : f'Base) return f'Base;
   --
   --  Cubic interpolation using a four point Lagrange polynomial
   --
   function lag4(p0, p1, p2, p3 : point; x : f'Base) return f'Base;
   --
   --  Quartic interpolation using a five point Lagrange polynomial
   --
   function lag5(p0, p1, p2, p3, p4 : point; x : f'Base) return f'Base;
end;
