with Ada.Text_IO;
package body BBS.Numerical.derivative is
   package float_io is new Ada.Text_IO.Float_IO(f'Base);
   -- --------------------------------------------------------------------------
   --
   --  Two point formula.  Use h > 0 for forward-difference and h < 0
   --  for backward-difference.  Derivative calculated at point x.
   --
   function pt2(f1 : test_func; x, h : f'Base) return f'Base is
   begin
      return (f1(x + h) - f1(x))/h;
   end;
   --
   --  Three point formulas.
   --
   function pt3a(f1 : test_func; x, h : f'Base) return f'Base is
   begin
      return (-3.0*f1(x) + 4.0*f1(x+h) - f1(x+2.0*h))/(2.0*h);
   end;
   --
   function pt3b(f1 : test_func; x, h : f'Base) return f'Base is
   begin
      return (f1(x+h) - f1(x-h))/(2.0*h);
   end;
   --
   --  Five point formulas.
   --
   function pt5a(f1 : test_func; x, h : f'Base) return f'Base is
   begin
      return (f1(x - 2.0*h) - 8.0*f1(x - h) + 8.0*f1(x + h) - f1(x + 2.0*h))/(12.0*h);
   end;
   --
   function pt5b(f1 : test_func; x, h : f'Base) return f'Base is
   begin
      return (-25.0*f1(x) + 48.0*f1(x + h) - 36.0*f1(x + 2.0*h) + 16.0*f1(x + 3.0*h) - 3.0*f1(x + 4.0*h))/(12.0*h);
   end;
   --
end BBS.Numerical.derivative;
