with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with test_cases;

procedure test_shell is
   selection : Integer := 0;
begin
   Ada.Text_IO.Put_Line("Numerical analysis routine tests.");
   loop
      Ada.Text_IO.Put_Line("Tests available are:");
      Ada.Text_IO.Put_Line("  1 - Numerical derivatives");
      Ada.Text_IO.Put_Line("  2 - Common functions");
      Ada.Text_IO.Put_Line("  3 - Interpolation");
      Ada.Text_IO.Put_Line("  4 - Numerical integration");
      Ada.Text_IO.Put("Selection: ");
      Ada.Integer_Text_IO.Get(selection, 0);
      --
      --  This is just to clear out any text on the rest of the line.
      --
      declare
         dummy : String := Ada.Text_IO.Get_line;
      begin
         null;  --  Nothing to do here.
      end;
      exit when (selection > 0) and (selection < 10);
   end loop;
   if selection = 1 then
      test_cases.test_derive;
   elsif selection = 2 then
      test_cases.test_func;
   elsif selection = 3 then
      test_cases.test_interp;
   elsif selection = 4 then
      test_cases.test_integ;
   end if;
end;
