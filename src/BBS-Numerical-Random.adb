package body BBS.Numerical.Random is
   --
   --  Linear congruent RNG.  You can configure the parameters as needed.
   --
   --  Note that while this fast, this is not a particularly good RNG.  However
   --  the choice of parameters can make the difference between dreadful
   --  and decent.  Whether this is a problem or not depends on your
   --  application.  Some variation of this is used as the default RNG
   --  in many libraries.
   --
   --  For example, the ANSI C committee provide the following parameters
   --  as "not recommended":  modulus = 2^32, a = 1103515245, c = 12345,
   --  then divided by 2^16.
   --
   procedure init(self : in out LCG) is
   begin
      self.seed := 1;
      self.modulus := 16#7FFF_FFFF#;
      self.a := 163490618;
      self.c := 0;
   end;
   --
   procedure init(self : in out LCG; seed, modulus, a, c : uint32) is
   begin
      self.seed := seed;
      self.modulus := modulus;
      self.a := a;
      self.c := c;
   end;
   --
   procedure setSeed(self : in out LCG; seed : uint32) is
   begin
      self.seed := seed;
   end;
   --
   function getNext(self : in out LCG) return uint32 is
   begin
      self.seed := uint32((uint64(self.a)*uint64(self.seed) + uint64(self.c)) mod uint64(self.modulus));
      return self.seed;
   end;
   --
   function getNextF(self : in out LCG) return double is
   begin
      return double(self.getNext)/double(self.modulus);
   end;
   --
   --  Mersenne Twister RNG.  The algorithm used is MT19937.
   --
   --  This has better statistical properties than the LCG.  Use this if
   --  that is important for your application.
   --
   procedure init(self : in out MT) is
   begin
      self.state(0) := 19650218;
      for i in 1 .. self.state'Last loop
         self.state(i) := (1812433253*(self.state(i-1) xor (self.state(i-1)/16#4000_0000#))) + uint32(i);
      end loop;
      self.index := 0;
   end;
   --
   procedure init(self : in out MT; seed : uint32) is
   begin
      self.state(0) := seed;
      for i in 1 .. self.state'Last loop
         self.state(i) := (1812433253*(self.state(i-1) xor (self.state(i-1)/16#4000_0000#))) + uint32(i);
      end loop;
      self.index := 0;
   end;
   --
   function getNext(self : in out MT) return uint32 is
      y : uint32;
   begin
      if self.index = 0 then
         self.MT_Generate;
      end if;
      y := self.state(self.index);
      if self.index = MT_Index'Last then
         self.index := 0;
      else
         self.index := self.index + 1;
      end if;
      y := y xor (y/16#0400#);
      y := y xor ((y*16#40#) and 2636928640);
      y := y xor ((y*16#4000#) and 4022730752);
      y := y xor (y/16#0002_0000#);
      return y;
   end;
   --
   function getNextF(self : in out MT) return double is
   begin
      return double(self.getNext)/double(16#1_0000_0000#);
   end;
   --
   procedure MT_generate(self : in out MT) is
      y : uint32;
   begin
      for i in MT_Index'range loop
         y := (self.state(i) and MT_bit32) +
              (self.state(MT_index((uint32(i) + 1) mod self.state'Length)) and MT_bits31);
         self.state(i) := self.state(MT_index((uint32(i) + 397) mod self.state'Length)) xor y/2;
         if (y mod 2) = 1 then
            self.state(i) := self.state(i) xor 2567483615;
         end if;
      end loop;
      null;
   end;
end;
