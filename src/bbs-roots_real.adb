--with Ada.Text_IO;
--with Ada.Float_Text_IO;
with Ada.Numerics.Generic_Elementary_Functions;
package body BBS.roots_real is
   package elem is new Ada.Numerics.Generic_Elementary_Functions(f);
   --
   --  Bisection algorithm for finding a root.
   --
   function bisection(test : test_func; lower, upper : in out f; limit : Positive; err : out errors) return f is
      val_high   : f;
      val_low    : f;
      val_mid    : f;
      mid_limit  : f;
   begin
      val_high := test(upper);
      val_low  := test(lower);
      if (val_high * val_low) > 0.0 then
         err := bad_args;
         return 0.0;
      end if;
      err := none;
      for i in 0 .. limit loop
         mid_limit := (lower + upper)/2.0;
         val_mid := test(mid_limit);
         exit when val_mid = 0.0;
         if (val_high * val_mid) > 0.0 then
            upper := mid_limit;
            val_high := val_mid;
         else
            lower := mid_limit;
            val_low := val_mid;
         end if;
      end loop;
      return (lower + upper)/2.0;
   end;
   --
   function seacant(test : test_func; lower, upper : in out f; limit : Positive; err : out errors) return f is
      val_high   : f;
      val_low    : f;
      val_mid    : f;
      mid_limit  : f;
   begin
      val_high := test(upper);
      val_low  := test(lower);
      if (val_high * val_low) > 0.0 then
         err := bad_args;
         return 0.0;
      end if;
      err := none;
      for i in 0 .. limit loop
         if val_high = val_low then
            err := no_solution;
            return 0.0;
         end if;
         mid_limit := upper - val_high*(upper - lower)/(val_high - val_low);
         val_mid := test(mid_limit);
         exit when val_mid = 0.0;
         if (val_high * val_mid) > 0.0 then
            upper := mid_limit;
            val_high := val_mid;
         else
            lower := mid_limit;
            val_low := val_mid;
         end if;
      end loop;
      if val_mid = 0.0 then
         upper := mid_limit;
         lower := mid_limit;
      end if;
      if val_high = val_low then
         err := no_solution;
         return 0.0;
      end if;
      return upper - val_high*(upper - lower)/(val_high - val_low);
   end;
   --
   function mueller(test : test_func; x0, x2 : in out f; limit : Positive; err : out errors) return f is
      x1 : f := (x0 + x2)/2.0;
      h1 : f := x1 - x0;
      h2 : f := x2 - x1;
      d1 : f := (test(x1) - test(x0))/h1;
      d2 : f := (test(x2) - test(x1))/h2;
      d_small : f := (d2 - d1) / (h2 + h1);
      d_big : f;
      b : f;
      h : f;
      e : f;
      root : f;
   begin
      err := none;
      for i in 0 .. limit loop
         b := d2 + (h2 * d_small);
         d_big := elem.Sqrt(b*b - (4.0*d_small*test(x2)));
         if abs(b - d_big) < abs(b + d_big) then
            e := b + d_big;
         else
            e := b - d_big;
         end if;
         h := -2.0*test(x2)/e;
         root := x2 + h;
         if h = 0.0 then
            return root;
         end if;
         x0 := x1;
         x1 := x2;
         x2 := root;
         h1 := x1 - x0;
         h2 := x2 - x1;
         d1 := (test(x1) - test(x2))/h1;
         d2 := (test(x2) - test(x1))/h2;
         d_small := (d2 - d1)/(h2 + h1);
      end loop;
      return root;
   end;
end;
