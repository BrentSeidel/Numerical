with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Numerics.Generic_Complex_Types;
with Ada.Numerics.Generic_Complex_Elementary_Functions;
with Ada.Text_IO;
package body BBS.Numerical.roots_complex is
   package ada_cmplx is new Ada.Numerics.Generic_Complex_Types(f'Base);
   package cmplx_elem is new Ada.Numerics.Generic_Complex_Elementary_Functions(ada_cmplx);
   package float_io is new Ada.Text_IO.Float_IO(f'Base);
   --
   function mueller(test : test_func; x0, x2 : in out cmplx.complex;
            limit : Positive; err : out errors) return cmplx.complex is
      x1      : cmplx.complex := (x0 + x2)/2.0;
      step1   : cmplx.complex := x1 - x0;
      step2   : cmplx.complex := x2 - x1;
      delta1  : cmplx.complex := (test(x1) - test(x0))/step1;
      delta2  : cmplx.complex := (test(x2) - test(x1))/step2;
      d_small : cmplx.complex := (delta2 - delta1) / (step2 + step1);
      d_big   : cmplx.complex;
      discriminant : cmplx.complex;
      b       : cmplx.complex;
      value   : cmplx.complex;
      e       : cmplx.complex;
      root    : cmplx.complex;
      temp    : cmplx.complex;
      nTwo    : constant cmplx.complex := (-2.0)*cmplx.one;
      atemp    : ada_cmplx.Complex;
   begin
      err := none;
      for i in 0 .. limit loop
         b := delta2 + (step2 * d_small);
         discriminant := b*b - (4.0*d_small*test(x2));
         atemp := (roots_complex.F(discriminant.r), roots_complex.F(discriminant.i));
         atemp := cmplx_elem.Sqrt(atemp);
         d_big := (r => cmplx.F(ada_cmplx.Re(atemp)), i => cmplx.F(ada_cmplx.Im(atemp)));
         if cmplx.magnitude(b - d_big) < cmplx.magnitude(b + d_big) then
            e := b + d_big;
         else
            e := b - d_big;
         end if;
         value := nTwo*test(x2)/e;
         root := x2 + value;
         if (cmplx.magnitude(value) = 0.0) or (x0 = x1) or (root = x2) then
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
end BBS.Numerical.roots_complex;
