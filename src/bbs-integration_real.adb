package body BBS.integration_real is
   --
   function trapezoid(test : test_func; lower, upper : f; steps : Positive) return f is
      step_size   : constant f := (upper - lower)/f(steps);
      accumulator : f := 0.0;
      last_func   : f := test(lower);
      next_func   : f;
      base        : f := lower;
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
   function simpson(test : test_func; lower, upper : f; steps : Positive) return f is
      step_size : constant f := (upper - lower)/f(2*steps);
      ends      : constant f := test(lower) + test(upper);
      sum1      : f := 0.0;
      sum2      : f := 0.0;
      base      : f := lower;
   begin
      for i in 1 .. steps - 1 loop
         base := base + step_size;
         sum1 := sum1 + test(base);
         base := base + step_size;
         sum2 := sum2+ test(base);
      end loop;
      base := base + step_size;
      sum1 := sum1 + test(base);
      return step_size*(ends + 2.0*sum2 + 4.0*sum1)/3.0;
   end;
   --
end;
