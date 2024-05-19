with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Text_IO;
package body BBS.Numerical.polynomial_real is
   package elem is new Ada.Numerics.Generic_Elementary_Functions(f);
   package float_io is new Ada.Text_IO.Float_IO(f'Base);
   --
   --  Utility function
   --
   function max(a, b : Natural) return Natural is
   begin
      if a > b then
         return a;
      else
         return b;
      end if;
   end;
   --
   --  Basic arithmatic operations
   --
   function "+" (Left, Right : poly) return poly is
      limit : constant Natural := max(Left'Last, Right'Last);
   begin
      declare
         res : poly(0 .. limit);
      begin
         for i in 0 .. limit loop
            if i > Left'Last then
               res(i) := Right(i);
            elsif i > Right'Last then
               res(i) := Left(i);
            else
               res(i) := Left(i) + Right(i);
            end if;
         end loop;
         return res;
      end;
   end;
   --
   function "-" (Left, Right : poly) return poly is
      limit : constant Natural := max(Left'Last, Right'Last);
   begin
      declare
         res : poly(0 .. limit);
      begin
         for i in 0 .. limit loop
            if i > Left'Last then
               res(i) := -Right(i);
            elsif i > Right'Last then
               res(i) := Left(i);
            else
               res(i) := Left(i) - Right(i);
            end if;
         end loop;
         return res;
      end;
   end;
   --
   function "*" (Left, Right : poly) return poly is
      limit : constant Natural := Left'Last + Right'Last;
   begin
      declare
         res : poly(0 .. limit) := (others => 0.0);
      begin
         for i in Left'Range loop
            for j in Right'Range loop
               res(i + j) := res(i + j) + Left(i)*Right(j);
            end loop;
         end loop;
         return res;
      end;
   end;
   --
   function "*" (Left : f; Right : poly) return poly is
      res : poly(Right'Range);
   begin
      for i in Right'Range loop
         res(i) := Left*Right(i);
      end loop;
      return res;
   end;
   --
   function "*" (Left : poly; Right : f) return poly is
      res : poly(Left'Range);
   begin
      for i in Left'Range loop
         res(i) := Left(i)*Right;
      end loop;
      return res;
   end;
   --
   function evaluate(Self : in poly; x : f'Base) return f'Base is
      accum : f'Base := 0.0;
   begin
      for i in reverse 1 .. Self'Last loop
         accum := x*(accum + self(i));
      end loop;
      return accum + Self(0);
   end;
   --
end BBS.Numerical.polynomial_real;
