--
--  This package contains routines for filtering and smoothing sets of data
--
generic
  type F is digits <>;
package BBS.Numerical.filter is
   type data_array is array (Integer range <>) of F'Base;
   --
   --  Type for an averaging filter.  The filter's values are all
   --  initialized to zero, which may cause issues with the first
   --  filter size data points.
   --
   type average(size : Positive) is tagged private;
   --
   --  Compute a value using the current and previous values with an
   --  averaging filter.
   --
   function filter(self : in out average; d : F'Base) return F'Base;

private
   type average(size : Positive) is tagged record
      data : data_array(1 .. size) := (others => 0.0);
   end record;
end BBS.Numerical.filter;
