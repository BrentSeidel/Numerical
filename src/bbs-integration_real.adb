package body BBS.integration_real is
   --
   function trapezoid(test : test_func; lower, upper : f; steps : Positive) return f is
      accumulator : f := 0.0;
      last_func   : f := test(lower);
      next_func   : f;
      base        : f := lower;
      step_size   : f := (upper - lower)/f(steps);
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
end;
