with Ada.Text_IO;
package body BBS.Numerical.interpolation is
   --
   --  Linear interpolation
   --
   --  Note that if many interpolations need to be done for the same two
   --  points, it may be more efficient to construct a first order poly-
   --  nomial and evaluate it at the various points.  c1 and c0 below
   --  would be the coefficients of the polynomial.
   --
   --  The Lagrange coefficients:
   --      l0 : constant f'Base := (x - p1.x) / (p0.x - p1.x);
   --      l1 : constant f'Base := (x - p0.x) / (p1.x - p0.x);
   --  Are converted to polynomial coeficients through the use of algebra.
   --
   function linear(p0, p1: point; x : f'Base) return f'Base is
      c1 : constant f'Base := (p0.y - p1.y) / (p0.x - p1.x);
      c0 : constant f'Base := (p0.x*p1.y - p1.x*p0.y) / (p0.x - p1.x);
   begin
      return x*c1 + c0;
   end;
   --
   --  Quadratic interpolation.  This uses a three point Lagrange polynomial.
   --
   function quadratic(p0, p1, p2 : point; x : f'Base) return f'Base is
      l0 : constant f'Base := ((x - p1.x)*(x - p2.x))/((p0.x - p1.x)*(p0.x - p2.x));
      l1 : constant f'Base := ((x - p0.x)*(x - p2.x))/((p1.x - p0.x)*(p1.x - p2.x));
      l2 : constant f'Base := ((x - p0.x)*(x - p1.x))/((p2.x - p0.x)*(p2.x - p1.x));
   begin
      return p0.y*l0 + p1.y*l1 + p2.y*l2;
   end;
end;
