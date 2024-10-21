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
   --  Natural log of Gamma function based on based on Lanczos approximation
   --  given in Numerical Recipes in C.  This should work for all real numbers
   --  greater than zero.
   --
   --  This has been compared with lngamma2n using single precision and
   --  gives the same results for 2*n from 0 to 100.  So I have confidence
   --  that the first few digits of the coefficents are correct.  It is
   --  possible that errors crept in with copying the later digits.
   --
   function lngamma(n : f'Base) return f'Base is
      --
      --  Note that this list of coefficients can't just be truncated to
      --  use fewer.  They must be recalculated.  The wikipedia page at
      --  https://en.wikipedia.org/wiki/Lanczos_approximation shows a
      --  Python example with different numbers of coefficients.
      --
      coeff : constant array (0 .. 5) of f'Base := (76.18009172947146, -86.50532032941677,
         24.01409824083091, -1.231739572450155, 0.1208650973866179e-2,
         -0.5395239384953e-5);
      temp : f'Base := n + 5.5;
      y    : f'Base := n;
      ser  : f'Base := 1.000000000190015;
   begin
      temp := temp - (n+0.5)*elem.Log(temp);
      for j in 0 .. 5 loop
         y := y + 1.0;
         ser := ser + coeff(j)/y;
      end loop;
      return -temp + elem.Log(2.506628274631005*ser/n);
   end;
   --
   --  Helper functions for incomplete Gamma functions.  This must be
   --  called with x > 0.0.  This uses the summation series.
   --
   function gser(a, x, lng : f'Base) return f'Base is
      err : constant f'Base := 3.0e-7;
      sum : f'Base := 1.0/a;
      del : f'Base := 1.0/a;
      ap  : f'Base := a;
   begin
      for n in 1 .. 100 loop
         ap  := ap + 1.0;
         del := del*x/ap;
         sum := sum + del;
         if (abs del) < (abs sum)*err then
            return sum*elem.Exp(-x*a*elem.Log(x)-lng);
         end if;
      end loop;
      return -1.0;
   end;
   --
   --  Helper function for incomplete Gamma functions.  This must be
   --  called with a > 0.0.  This uses the continued fraction.
   --
   function gcf(a, x, lng : f'Base) return f'Base is
      err : constant f'Base := 3.0e-7;
      b   : f'Base := x - a + 1.0;
      c   : f'Base := 1.0/f'Base'Model_Small;
      d   : f'Base := 1.0/b;
      h   : f'Base := d;
      del : f'Base;
      an  : f'Base;
   begin
      for n in 1 .. 100 loop
         an  := -f'Base(n)*(f'Base(n) - a);
         b   := b + 2.0;
         d   := an*d + b;
         if (abs d) < f'Base'Model_Small then
            d := f'Base'Model_Small;
         end if;
         c := b + an/c;
         if (abs c) < f'Base'Model_Small then
            c := f'Base'Model_Small;
         end if;
         d   := 1.0/d;
         del := d*c;
         h   := h*del;
         if (abs (del - 1.0)) < err then
            return elem.exp(-x * a*elem.Log(x) - lng)*h;
         end if;
      end loop;
      return -1.0;
   end;
   --
   --  Regularized incomplete upper Gamma function.
   --
   function gammaP(a, x : f'Base) return f'Base is
      lng  : constant f'Base := lngamma(a);
   begin
      if x < a+1.0 then
         return gser(a, x, lng);
      else
         return 1.0 - gcf(a, x, lng);
      end if;
   end;
   --
   --  Regularized incomplete lower Gamma function.
   --
   function gammaQ(a, x : f'Base) return f'Base is
      lng : constant f'Base := lngamma(a);
   begin
      if x < a+1.0 then
         return 1.0 - gser(a, x, lng);
      else
         return gcf(a, x, lng);
      end if;
   end;
   --
   --  Compute the factorial of a number.  This will overflow Float at n = 35.
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
