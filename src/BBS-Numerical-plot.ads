--
--  This is the base class for plotting.  It describes the interface that
--  all concrete plotting classes need to provide.  There should also be
--  an initialization and termination procedures, but these would be
--  implementation specific, so they can't be described here.
--
package BBS.Numerical.Plot is
   type point is record
      x : Float;
      y : Float;
   end record;
   --
   type point_list is array (Integer range <>) of point;
   --
   --  Define a plot data type
   --
   type plot_record is tagged limited private;
   type plot is access all plot_record;
   --
   --  Known glyphs to plot
   --
   type glyph is (glyph_plus, glyph_X, glyph_asterisk, glyph_box, glyph_diamond,
                  glyph_octagon);
   --
   --  Draw the plot frame
   --
   procedure frame(self : in out plot_record; xTicks, yTicks : Natural;
                  xLines , yLines : Boolean);
   --
   --  Set Axis label
   --
   procedure label(self : in out plot_record; xLabel, yLabel : String);
   --
   --  Set title
   --
   procedure title(self : in out plot_record; title : String);
   --
   --  Plot lines
   --
   procedure draw_line(self : in out plot_record; points : point_list;
                        color : String);
   --
   --  Plot point(s)
   --
   procedure draw_point(self : in out plot_record; p : point;
                        size : Positive; color : String);
   procedure draw_point(self : in out plot_record; points : point_list;
                        size : Positive; color : String);
   --
   --  Draw text at a point
   --
   procedure draw_text(self : in out plot_record; p : point;
                        color, text : String);
   --
   --  Draw a glyph at a point
   --
   procedure draw_glyph(self : in out plot_record; p : point; g : glyph;
                        color : String);
   procedure draw_glyph(self : in out plot_record; points : point_list;
                        g : glyph; color : String);
private
   type plot_record is tagged limited record
      valid : Boolean := False;
   end record;

   Unimplemented : exception;
end BBS.Numerical.Plot;
