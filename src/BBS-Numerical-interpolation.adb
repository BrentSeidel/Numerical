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
   function lag2(p0, p1: point; x : f'Base) return f'Base is
      c1 : constant f'Base := (p0.y - p1.y) / (p0.x - p1.x);
      c0 : constant f'Base := (p0.x*p1.y - p1.x*p0.y) / (p0.x - p1.x);
   begin
      return x*c1 + c0;
   end;
   --
   --  Quadratic interpolation.  This uses a three point Lagrange polynomial.
   --
   function lag3(p0, p1, p2 : point; x : f'Base) return f'Base is
      l0 : constant f'Base := ((x - p1.x)*(x - p2.x))/((p0.x - p1.x)*(p0.x - p2.x));
      l1 : constant f'Base := ((x - p0.x)*(x - p2.x))/((p1.x - p0.x)*(p1.x - p2.x));
      l2 : constant f'Base := ((x - p0.x)*(x - p1.x))/((p2.x - p0.x)*(p2.x - p1.x));
   begin
      return p0.y*l0 + p1.y*l1 + p2.y*l2;
   end;
   --
   --  Cubic interpolation.  This uses a four point Lagrange polynomial
   --
   function lag4(p0, p1, p2, p3 : point; x : f'Base) return f'Base is
      l0 : constant f'Base := ((x - p1.x)*(x - p2.x)*(x - p3.x))/((p0.x - p1.x)*(p0.x - p2.x)*(p0.x - p3.x));
      l1 : constant f'Base := ((x - p0.x)*(x - p2.x)*(x - p3.x))/((p1.x - p0.x)*(p1.x - p2.x)*(p1.x - p3.x));
      l2 : constant f'Base := ((x - p0.x)*(x - p1.x)*(x - p3.x))/((p2.x - p0.x)*(p2.x - p1.x)*(p2.x - p3.x));
      l3 : constant f'Base := ((x - p0.x)*(x - p1.x)*(x - p2.x))/((p3.x - p0.x)*(p3.x - p1.x)*(p3.x - p2.x));
   begin
      return p0.y*l0 + p1.y*l1 + p2.y*l2 + p3.y*l3;
   end;
   --
   --  Quartic interpolation using a five point Lagrange polynomial
   --
   function lag5(p0, p1, p2, p3, p4 : point; x : f'Base) return f'Base is
      l0 : constant f'Base := ((x - p1.x)*(x - p2.x)*(x - p3.x)*(x - p4.x))/
                              ((p0.x - p1.x)*(p0.x - p2.x)*(p0.x - p3.x)*(p0.x - p4.x));
      l1 : constant f'Base := ((x - p0.x)*(x - p2.x)*(x - p3.x)*(x - p4.x))/
                              ((p1.x - p0.x)*(p1.x - p2.x)*(p1.x - p3.x)*(p1.x - p4.x));
      l2 : constant f'Base := ((x - p0.x)*(x - p1.x)*(x - p3.x)*(x - p4.x))/
                              ((p2.x - p0.x)*(p2.x - p1.x)*(p2.x - p3.x)*(p2.x - p4.x));
      l3 : constant f'Base := ((x - p0.x)*(x - p1.x)*(x - p2.x)*(x - p4.x))/
                              ((p3.x - p0.x)*(p3.x - p1.x)*(p3.x - p2.x)*(p3.x - p4.x));
      l4 : constant f'Base := ((x - p0.x)*(x - p1.x)*(x - p2.x)*(x - p3.x))/
                              ((p4.x - p0.x)*(p4.x - p1.x)*(p4.x - p2.x)*(p4.x - p3.x));
   begin
      return p0.y*l0 + p1.y*l1 + p2.y*l2 + p3.y*l3 + p4.y*l4;
   end;
end;
