--with Ada.Text_IO;
--with Ada.Float_Text_IO;
with Ada.Numerics.Generic_Elementary_Functions;
package body BBS.Numerical.Statistics is
   --
   --  Compute the mean of an array of data
   --
   function mean(d : data_array) return F is
      sum : F := 0.0;
   begin
      for i in d'Range loop
         sum := sum + d(i);
      end loop;
      return sum/F(d'Length);
   end;
   --
   --  Compute the limits of an array of data
   --
   procedure limits(d : data_array; min : out F; max : out F) is
   begin
      min := d(d'First);
      max := d(d'First);
      for i in (d'First + 1) .. d'Last loop
         if d(i) > max then
            max := d(i);
         end if;
         if d(i) < min then
            min := d(i);
         end if;
      end loop;
   end;
   --
   --  Compute the variance (and mean) of an array of data.  Use this,
   --  instead of mean() if you need both values.
   --
   procedure variance(d : data_array; var : out F; mean : out F) is
      sum2 : F := 0.0;
      sum  : F := 0.0;
   begin
      for i in d'Range loop
         sum  := sum + d(i);
         sum2 := sum2 + (d(i)*d(i));
      end loop;
      mean := sum/F(d'Length);
      var  := (sum2 - sum*sum/F(d'Length))/F(d'Length - 1);
   end;
end;
