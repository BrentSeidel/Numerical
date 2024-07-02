--
--  Contains code for pseudo-random number generators.
--
package BBS.Numerical.Random is
   --
   --  The abstract random number generator.  All RNGs will provide
   --  these operations.
   --
   type double is digits 12;
   type RNG is abstract tagged private;
   procedure init(self : in out RNG) is abstract;
   --
   --  Returns the next random number as uint32
   --
   function getNext(self : in out RNG) return uint32 is abstract;
   --
   --  Returns the next random number as a real number from 0.0 to 1.0.
   function getNextF(self : in out RNG) return double is abstract;
   --
   --  Linear congruent RNG.  You can configure the parameters as needed.
   --
   type LCG is new RNG with private;
   overriding
   procedure init(self : in out LCG);
   procedure init(self : in out LCG; seed, modulus, a, c : uint32);
   procedure setSeed(self : in out LCG; seed : uint32);
   overriding
   function getNext(self : in out LCG) return uint32;
   overriding
   function getNextF(self : in out LCG) return double;
   --
   --  Mersenne Twister RNG.  The algorithm used is MT19937.
   --
   type MT is new RNG with private;
   overriding
   procedure init(self : in out MT);
   procedure init(self : in out MT; seed : uint32);
   overriding
   function getNext(self : in out MT) return uint32;
   overriding
   function getNextF(self : in out MT) return double;
private
   --
   type RNG is abstract tagged record
      null;
   end record;
   --
   type LCG is new RNG with record
      seed : uint32;
      modulus : uint32;
      a    : uint32;
      c    : uint32;
   end record;
   --
   type MT_index is new Integer range 0 .. 623;
   type MT_state is array (MT_index'Range) of uint32;
   type MT is new RNG with record
      state : MT_state;
      index : MT_index;
   end record;
   MT_bits32 : constant uint32 := 16#FFFF_FFFF#;
   MT_bits31 : constant uint32 := 16#7FFF_FFFF#;
   MT_bit32  : constant uint32 := 16#8000_0000#;
   --
   procedure MT_generate(self : in out MT);
end;
