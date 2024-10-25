with Ada.Text_IO;
package body BBS.Numerical.integration_real is
   package float_io is new Ada.Text_IO.Float_IO(f'Base);
   --
   --  Integrate the provided function between the lower and upper limits using
   --  the composite midpoint method with the specified number of steps.
   --
   function midpoint(test : test_func; lower, upper : f'Base; steps : Positive) return f'Base is
      step_size   : constant f'Base := (upper - lower)/f(steps);
      accumulator : f'Base := 0.0;
      value       : f'Base;
      base        : f'Base := lower + step_size/2.0;
   begin
      for i in 0 .. steps - 1 loop
         base := base + step_size;
         value := test(base);
         accumulator := accumulator + value*step_size;
      end loop;
      return accumulator;
   end;
   --
   --  Integrate the provided function between the lower and upper limits using
   --  the composite trapezoid method with the specified number of steps.
   --
   function trapezoid(test : test_func; lower, upper : f'Base; steps : Positive) return f'Base is
      step_size   : constant f'Base := (upper - lower)/f(steps);
      accumulator : f'Base := 0.0;
      last_func   : f'Base := test(lower);
      next_func   : f'Base;
      base        : f'Base := lower;
   begin
      for i in 0 .. steps - 1 loop
         base := base + step_size;
         next_func := test(base);
         accumulator := accumulator + ((next_func + last_func)/2.0)*step_size;
         last_func := next_func;
      end loop;
      return accumulator;
   end;
   --
   --  Integrate the provided function between the lower and upper limits using
   --  adaptive trapeziod integration.  A requested tolerance is input and
   --  returned as an estimated value.
   --
   function adapt_trap(test : test_func; lower, upper : f'Base; tol : in out f'Base;
                          levels : Natural) return f'Base is
      --
      --  Function to recurse for adaptive trapeziod integration
      --
      function interval(t : test_func; lower, upper : f'Base; tol : in out f'Base;
                        t_l, t_u, full : f'Base; levels : Natural) return f'Base is
         mid  : constant f'Base := (upper + lower)/2.0;
         t_m  : constant f'Base := test((lower + upper)/2.0);
         l    : f'Base;
         r    : f'Base;
         tol1 : f'Base := tol/2.0;
         tol2 : f'Base := tol/2.0;
      begin
         l := (mid - lower)*(t_l + t_m)/2.0;
         r := (upper - mid)*(t_m + t_u)/2.0;
         if (abs(l + r - full) > tol) and (levels > 0) then
            l := interval(test, lower, mid, tol1, t_l, t_m, l, levels - 1);
            r := interval(test, mid, upper, tol2, t_m, t_u, r, levels - 1);
            tol := tol1 + tol2;
         else
            tol := abs(l + r - full);
         end if;
         return l + r;
      end;
      --
      mid  : constant f'Base := (upper + lower)/2.0;
      t_l  : constant f'Base := test(lower);
      t_u  : constant f'Base := test(upper);
      t_m  : constant f'Base := test(mid);
      full : constant f'Base := (upper - lower)*(t_l + t_u)/2.0;
      l    : f'Base;
      r    : f'Base;
      tol1 : f'Base := tol/2.0;
      tol2 : f'Base := tol/2.0;
   begin
      l := (mid - lower)*(t_l + t_m)/2.0;
      r := (upper - mid)*(t_m + t_u)/2.0;
      if (abs(l + r - full) > tol) and (levels > 0) then
         l := interval(test, lower, mid, tol1, t_l, t_m, l, levels - 1);
         r := interval(test, mid, upper, tol2, t_m, t_u, r, levels - 1);
         tol := tol1 + tol2;
      else
         tol := abs(l + r - full);
      end if;
      return l + r;
   end;
   --
   --  Integrate the provided function between the lower and upper limits using
   --  the composite Simpson's rule with the specified number of steps.  Note
   --  that since Simpson's rule evaluates the function in the midpoint of a
   --  segment, the effective number of steps is doubled.
   --
   function simpson(test : test_func; lower, upper : f'Base; steps : Positive) return f'Base is
      step_size : constant f'Base := (upper - lower)/f'Base(2*steps);
      ends      : constant f'Base := test(lower) + test(upper);
      sum1      : f'Base := 0.0;
      sum2      : f'Base := 0.0;
      base      : f'Base := lower;
   begin
      for i in 1 .. steps - 1 loop
         base := base + step_size;
         sum1 := sum1 + test(base);  --  Odd steps
         base := base + step_size;
         sum2 := sum2 + test(base);  --  Even steps
      end loop;
      base := base + step_size;
      sum1 := sum1 + test(base);
      return step_size*(ends + 2.0*sum2 + 4.0*sum1)/3.0;
   end;
   --
   --  Integrate the provided function between the lower and upper limits using
   --  adaptive Simpson's integration.
   --
   function adapt_simpson(test : test_func; lower, upper : f'Base; tol : in out f'Base;
                          levels : Natural) return f'Base is
      --
      --  Function to recurse for adaptive Simpson's integration
      --
      function interval(t : test_func; lower, upper : f'Base; tol : in out f'Base;
                        t_l, t_m, t_u, full : f'Base; levels : Natural) return f'Base is
         mid  : constant f'Base := (upper + lower)/2.0;
         t_ml : constant f'Base := test((lower + mid)/2.0);
         t_mu : constant f'Base := test((mid + upper)/2.0);
         l    : f'Base;
         r    : f'Base;
         tol1 : f'Base := tol/2.0;
         tol2 : f'Base := tol/2.0;
      begin
         l := (mid - lower)*(t_l + 4.0*t_ml + t_m)/6.0;
         r := (upper - mid)*(t_m + 4.0*t_mu + t_u)/6.0;
         if (abs(l + r - full) > tol) and (levels > 0) then
            l := interval(test, lower, mid, tol1, t_l, t_ml, t_m, l, levels - 1);
            r := interval(test, mid, upper, tol2, t_m, t_mu, t_u, r, levels - 1);
            tol := tol1 + tol2;
         else
            tol := abs(l + r - full);
         end if;
         return l + r;
      end;
      --
      mid  : constant f'Base := (upper + lower)/2.0;
      t_l  : constant f'Base := test(lower);
      t_u  : constant f'Base := test(upper);
      t_m  : constant f'Base := test(mid);
      t_ml : constant f'Base := test((lower + mid)/2.0);
      t_mu : constant f'Base := test((mid + upper)/2.0);
      full : constant f'Base := (upper - lower)*(t_l + 4.0*t_m + t_u)/6.0;
      l    : f'Base;
      r    : f'Base;
      tol1 : f'Base := tol/2.0;
      tol2 : f'Base := tol/2.0;
   begin
      l := (mid - lower)*(t_l + 4.0*t_ml + t_m)/6.0;
      r := (upper - mid)*(t_m + 4.0*t_mu + t_u)/6.0;
      if (abs(l + r - full) > tol) and (levels > 0) then
         l := interval(test, lower, mid, tol1, t_l, t_ml, t_m, l, levels - 1);
         r := interval(test, mid, upper, tol2, t_m, t_mu, t_u, r, levels - 1);
         tol := tol1 + tol2;
      else
         tol := abs(l + r - full);
      end if;
      return l + r;
   end;
   --
   --  Integrate the provided function between the lower and upper limits using
   --  Romberg's method.  This repeatedly uses the trapezoid method and applies
   --  Richardson extrapolation to arrive at the final answer.
   --
   type selection is mod 2;
   function romberg(test : test_func; lower, upper : f'Base; steps : Positive) return f'Base is
      buff : array (selection , 0 .. steps - 1) of f'Base := (others => (others => 0.0));
      sel  : selection := 1;
      size : f'Base := upper - lower;
      max  : Positive := 1;
      acc  : f'Base;
      n_k  : f'Base;
   begin
      buff(0, 0) := (test(lower) + test(upper))*size/2.0;

      for i in 1 .. steps - 1 loop
         size := size/2.0;
         acc  := 0.0;
         for j in 1 .. max loop
            acc := acc + test(lower + f'Base(2*j-1)*size);
         end loop;
         buff(sel, 0) := size*acc + 0.5*buff(sel+1, 0);

         for j in 1 .. i loop
            n_k := 4.0**j;
            buff(sel, j) := (n_k*buff(sel, j - 1) - buff(sel + 1, j - 1))/(n_k - 1.0);
         end loop;
         --
         --  For the comparizon, equality with 0.0 can be replaced by an allowable
         --  tolerance.
         --
         if (i > 1) and (abs (buff(sel + 1, i - 1) - buff(sel, i))) = 0.0 then
            return buff(sel, i);
         end if;
         sel := sel + 1;
         max  := max*2;
      end loop;
      return buff(sel + 1, steps - 1);
   end;
   --
end;
