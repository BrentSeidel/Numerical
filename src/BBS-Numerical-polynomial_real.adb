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
   --  Division based on poldiv() in "Numerical Recipes in C", second
   --  edition, 1992 by William H Press, Saul A. Tuekolsky, William T.
   --  Vetterlink, and Brian P. Flannery, section 5.3.
   --
   --  u/v => q, r
   --
   procedure divide(u, v : poly; q : out poly; r : out poly) is
      temp : poly(u'Range) := u;
   begin
      q := (others => 0.0);
      r := (others => 0.0);
      --
      for k in reverse 0 .. u'Last - v'Last loop
         q(k) := temp(v'Last + k)/v(v'Last);
         for j in k .. v'Last + k - 1 loop
            temp(j) := temp(j) - q(k)*v(j-k);
         end loop;
      end loop;
      r(0 .. v'Last - 1) := temp(0 .. v'Last - 1);
   end;
   --
   --  Evaluate a polynomial at x.
   --
   function evaluate(p : poly; x : f'Base) return f'Base is
      accum : f'Base := 0.0;
   begin
      for i in reverse 1 .. p'Last loop
         accum := x*(accum + p(i));
      end loop;
      return accum + p(0);
   end;
   --
   --  Trims a polynomial by removing leading zero coefficients.
   --
   function trim(p : poly) return poly is
      limit : Natural := p'Last;
   begin
      while (p(limit) = 0.0) and (limit > 0) loop
         limit := limit - 1;
      end loop;
      declare
         res : poly(0 .. limit);
      begin
         for i in 0 .. limit loop
            res(i) := p(i);
         end loop;
         return res;
      end;
   end;
   --
   --  Return the order of a polynomial
   --
   function order(p : poly) return Natural is
      limit : Natural := p'Last;
   begin
      while (p(limit) = 0.0) and (limit > 0) loop
         limit := limit - 1;
      end loop;
      return limit;
   end;
   --
   --  Basic calculus
   --
   function integrate(p : poly; c : f'Base) return poly is
      res : poly(0 .. p'Last + 1);
   begin
      res(0) := c;
      for i in p'Range loop
         res(i + 1) := p(i)/(f'Base(i) + 1.0);
      end loop;
      return res;
   end;
   --
   function derivative(p : poly) return poly is
      limit : Natural;
   begin
      if p'Last = 0 then
         limit := 0;
      else
         limit := p'Last - 1;
      end if;
      Ada.Text_IO.Put_Line("Derivative: p'Last = " & Natural'Image(p'Last) &
         ", limit = " & Natural'Image(limit));
      declare
         res : poly(0 .. limit);
      begin
         if p'Last = 0 then
            res(0) := 0.0;
         else
            for i in 1 .. p'Last loop
               res(i - 1) := f'Base(i)*p(i);
            end loop;
         end if;
         return res;
      end;
   end;
   --
end BBS.Numerical.polynomial_real;
