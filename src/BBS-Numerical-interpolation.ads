generic
   type F is digits <>;
package BBS.Numerical.interpolation is

   type point is record
      x : f'Base;
      y : f'Base;
   end record;
   --
   --  Linear interpolation.
   --
   --  Note that this will be most accurate for x between the two points.
   --  Outside of that is extrapolation.
   --
   function linear(p0, p1: point; x : f'Base) return f'Base;
end;
