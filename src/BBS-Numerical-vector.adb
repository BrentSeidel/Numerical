with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Text_IO;
package body BBS.Numerical.vector is
   package elem is new Ada.Numerics.Generic_Elementary_Functions(f);
   package float_io is new Ada.Text_IO.Float_IO(f'Base);
   --
   --  Basic arithmatic operations
   --
   function "+" (Left, Right : vect) return vect is
      res : vect(Left'Range);
   begin
      for i in Left'Range loop
         res(i) := Left(i) + Right(i);
      end loop;
      return res;
   end;
   --
   function "-" (Left, Right : vect) return vect is
      res : vect(Left'Range);
   begin
      for i in Left'Range loop
         res(i) := Left(i) - Right(i);
      end loop;
      return res;
   end;
   --
   function "*" (Left, Right : vect) return f'Base is
      res : f'Base := 0.0;
   begin
      for i in Left'Range loop
         res := res + Left(i)*Right(i);
      end loop;
      return res;
   end;
   --
   function "*" (Left : f; Right : vect) return vect is
      res : vect(Right'Range);
   begin
      for i in Right'Range loop
         res(i) := Left*Right(i);
      end loop;
      return res;
   end;
   --
   function "*" (Left : vect; Right : f) return vect is
      res : vect(Left'Range);
   begin
      for i in Left'Range loop
         res(i) := Left(i)*Right;
      end loop;
      return res;
   end;
   --

   function magnitude(self : in vect) return f'Base is
      res : f'Base := 0.0;
   begin
      for i in self'Range loop
         res := res + self(i)*self(i);
      end loop;
      return elem.Sqrt(res);
   end;
   --
   --  Scale value to have a magnitude of 1.  This is effectively a
   --  unit vector.
   --
   function normalize(self : in vect) return vect is
      mag : f'Base := magnitude(self);
      res : vect(self'Range);
   begin
      for i in self'Range loop
         res(i) := self(i)/mag;
      end loop;
      return res;
   end;
   --
end BBS.Numerical.vector;
