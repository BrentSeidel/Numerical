with Ada.Text_IO;
with Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;
package body BBS.Numerical.functions is
   package float_io is new Ada.Text_IO.Float_IO(f'Base);
   package elem is new Ada.Numerics.Generic_Elementary_Functions(f'Base);
   --
   --  Compute the gamma function of a positive number divided by two.
   --  This is initially used by the chi-squared statistics function, but
   --  may have other applications.  The full gamma function will be
   --  implemented when needed.
   --
   --  Note that only gamma of positive and half positive integers are needed,
   --  i.e. 1/2, 1, 3/2, 2, 5/2, ...
   --  This will be easier to implement than the full gamma functions since
   --  gamma(1/2) is sqrt(pi), gamma(1) is 1, and gamma(a+1) is a*gamma(a).
   --
   function gamma2n(n : Positive) return f'Base is
      base : f'Base;
   begin
      --
      --  If n is even, then base is 1, otherwise it is sqrt(pi)
      --
      if 2*(n/2) = n then
         base := 1.0;
         for i in 2 .. (n/2) loop
            base := base*f'Base(i-1);
         end loop;
      else
         base := elem.sqrt(Ada.Numerics.Pi);
         for i in 1 .. (n/2) loop
            base := base*(f'Base(i)-0.5);
         end loop;
      end if;
      return base;
   end;
   --
   --  Return the natural logarithm of gamma2n.  This allows larger
   --  values of n without overflow.
   --
   function lngamma2n(n : Positive) return f'Base is
      base : f'Base;
   begin
      --
      --  If n is even, then base is 1, otherwise it is sqrt(pi)
      --
      if 2*(n/2) = n then
         base := 0.0;
         for i in 2 .. (n/2) loop
            base := base + elem.log(f'Base(i-1));
         end loop;
      else
         base := elem.log(elem.sqrt(Ada.Numerics.Pi));
         for i in 1 .. (n/2) loop
            base := base + elem.log((f'Base(i)-0.5));
         end loop;
      end if;
      return base;
   end;
   --
   --  Compute the factorial of a number.  This will probably overflow Float at
   --  around n = 35.
   --
   function factorial(n : Natural) return f'Base is
      base : f'Base := 1.0;  --  0! is defined as 1
   begin
      for i in 1 .. n loop
         base := base*f'Base(i);
      end loop;
      return base;
   end;
   --
   --  Compute the natural log of the factorial.  This will allow much larger
   --  values of n before overflowing.
   --
   function lnfact(n : Natural) return f'Base is
      base : f'Base := 0.0;  --  0! is defined as 1
   begin
      for i in 1 .. n loop
         base := base + elem.Log(f'Base(i));
      end loop;
      return base;
   end;
   --
   --  Compute the binomial coefficient - n choose k.  Note that the result is
   --  an integer value, but f'Base is used to allow greater range.
   --
   --   (n)     n!
   --   ( )  --------
   --   (k)  k!(n-k)!
   --
   --  Expanding the factorials and cancelling terms leads to the following
   --  formula.  Note that since it is symmetrical with respect to k, the limit
   --  for the product can be the lesser of k or (n-k).
   --
   --   (n)    k   n + 1 - i
   --   ( ) = PROD ---------
   --   (k)   i=1      i
   --
   function nChoosek(n, k : Natural) return f'Base is
      base : f'Base := 1.0;
      top  : constant f'Base := f'Base(n + 1);
      limit : Integer;
   begin
      if k < (n-k) then
         limit := k;
      else
         limit := n - k;
      end if;
      for i in 1 .. limit loop
         base := base*(top - f'Base(i))/f'Base(i);
      end loop;
      return base;
   end;
   --
end BBS.Numerical.functions;
