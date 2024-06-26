with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Text_IO;
package body BBS.Numerical.roots_real is
   package elem is new Ada.Numerics.Generic_Elementary_Functions(f);
   package float_io is new Ada.Text_IO.Float_IO(f'Base);
   --
   --  Bisection algorithm for finding a root.
   --
   function bisection(test : test_func; lower, upper : in out f'Base; limit : Positive; err : out errors) return f'Base is
      val_high   : f'Base;
      val_low    : f'Base;
      val_mid    : f'Base;
      mid_limit  : f'Base;
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
   function seacant(test : test_func; lower, upper : in out f'Base; limit : Positive; err : out errors) return f'Base is
      val_high   : f'Base;
      val_low    : f'Base;
      val_mid    : f'Base;
      mid_limit  : f'Base;
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
   function mueller(test : test_func; x0, x2 : in out f'Base; limit : in out Positive; err : out errors) return f'Base is
      x1      : f'Base := (x0 + x2)/2.0;
      step1   : f'Base := x1 - x0;
      step2   : f'Base := x2 - x1;
      delta1  : f'Base := (test(x1) - test(x0))/step1;
      delta2  : f'Base := (test(x2) - test(x1))/step2;
      d_small : f'Base := (delta2 - delta1) / (step2 + step1);
      d_big   : f'Base;
      discriminant : f'Base;
      b       : f'Base;
      value   : f'Base;
      e       : f'Base;
      root    : f'Base := x2;
      temp    : f'Base;
      epsilon : constant f'Base := 1.0e-10;  --  Adjust as needed.  Maybe make a parameter
   begin
      err := none;
      for i in 1 .. limit loop
         b := delta2 + (step2 * d_small);
         discriminant := b*b - (4.0*d_small*test(x2));
         if discriminant < 0.0 then
            err := no_solution;
            limit := i;
            return root;
         end if;
         d_big := elem.Sqrt(discriminant);
         if abs(b - d_big) < abs(b + d_big) then
            e := b + d_big;
         else
            e := b - d_big;
         end if;
         value := -2.0*test(x2)/e;
         root := x2 + value;
         if (abs(value) <= epsilon) or (x0 = x1) or (root = x2) then
            limit := i;
            return root;
         end if;
         x0 := x1;
         x1 := x2;
         x2 := root;
         step1 := x1 - x0;
         step2 := x2 - x1;
         temp := test(x1);
         delta1 := (temp - test(x0))/step1;
         delta2 := (test(x2) - temp)/step2;
         d_small := (delta2 - delta1)/(step2 + step1);
      end loop;
      return root;
   end;
end;
