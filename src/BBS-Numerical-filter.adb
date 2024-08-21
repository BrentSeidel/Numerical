package body BBS.Numerical.filter is
   --
   --  Moving average filter
   --
   function filter(self : in out average; d : F'Base) return F'Base is
      temp : F'Base := d;
   begin
      for x in 1 .. self.size loop
         temp := temp + self.data(x);
      end loop;
      for x in reverse 2 .. self.size loop
         self.data(x) := self.data(x - 1);
      end loop;
      self.data(1) := d;
      return temp/F'Base(self.size);
   end;
end BBS.Numerical.filter;
