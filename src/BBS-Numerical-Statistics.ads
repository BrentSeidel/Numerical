generic
   type F is digits <>;
package BBS.Numerical.statistics is
   type data_array is array (Integer range <>) of F;
   --
   --  Compute the mean of an array of data
   --
   function mean(d : data_array) return F;
   --
   --  Compute the median of an array of data
   --
   procedure limits(d : data_array; min : out F; max : out F);
   --
   --  Compute the variance (and mean) of an array of data.  Use this,
   --  instead of mean() if you need both values.
   --
   procedure variance(d : data_array; var : out F; mean : out F);
end;
