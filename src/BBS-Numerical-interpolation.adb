with Ada.Text_IO;
package body BBS.Numerical.interpolation is
   --
   --  Linear interpolation
   --
   function linear(p0, p1: point; x : f'Base) return f'Base is
      l0 : constant f'Base := (x - p1.x) / (p0.x - p1.x);
      l1 : constant f'Base := (x - p0.x) / (p1.x - p0.x);
   begin
      return l0*p0.y + l1*p1.y;
   end;
end;
