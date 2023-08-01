with Ada.Text_IO;
with Ada.Float_Text_IO;
package body BBS.roots_real is
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
      return upper - val_high*(upper - lower)/(val_high - val_low);
   end;
end;
