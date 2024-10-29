--
--  This is a concrete package for writing linear (as opposed to log)
--  plots to a svg file.
--
with Ada.Text_IO;
with BBS.Numerical.plot;
package BBS.Numerical.plot_svg_linear is
   type linear_svg_plot_record is new BBS.Numerical.plot.plot_record with private;
   --
   --  Open the named file and start the plot
   --
   procedure start_plot(self : in out linear_svg_plot_record; name : string;
                        xmin, xmax, ymin, ymax : float);
   --
   --  Draw the plot frame
   --
   overriding
   procedure frame(self : in out linear_svg_plot_record; xTicks, yTicks : Natural;
                  xLines , yLines : Boolean);
   --
   --  Set Axis label
   --
   overriding
   procedure label(self : in out linear_svg_plot_record; xLabel, yLabel : String);
   --
   --  Set title
   --
   overriding
   procedure title(self : in out linear_svg_plot_record; title : String);
   --
   --  Plot lines
   --
   overriding
   procedure draw_line(self : in out linear_svg_plot_record;
                     points : BBS.Numerical.plot.point_list; color : String);
   --
   --  Plot point(s)
   --
   overriding
   procedure draw_point(self : in out linear_svg_plot_record;
                        p : BBS.Numerical.plot.point; size : Float; color : String);
   overriding
   procedure draw_point(self : in out linear_svg_plot_record;
                        points : BBS.Numerical.plot.point_list; size : Float; color : String);
   --
   --  Draw text at a point
   --
   overriding
   procedure draw_text(self : in out linear_svg_plot_record;
                        p : BBS.Numerical.plot.point; color, text : String);
   --
   --  Draw a glyph at a point
   --
   overriding
   procedure draw_glyph(self : in out linear_svg_plot_record;
                        p : BBS.Numerical.plot.point; g : BBS.Numerical.plot.glyph; color : String);
   overriding
   procedure draw_glyph(self : in out linear_svg_plot_record;
                        points : BBS.Numerical.plot.point_list; g : BBS.Numerical.plot.glyph; color : String);
   overriding
   procedure draw_glyph(self : in out linear_svg_plot_record;
                        p : BBS.Numerical.plot.point_err; g : BBS.Numerical.plot.glyph; color : String);
   overriding
   procedure draw_glyph(self : in out linear_svg_plot_record;
                        points : BBS.Numerical.plot.point_err_list; g : BBS.Numerical.plot.glyph; color : String);
   --
   --  Close the plot
   --
   procedure end_plot(self : in out linear_svg_plot_record);
   --

private
   type linear_svg_plot_record is new BBS.Numerical.plot.plot_record with
      record
         io    : Ada.Text_IO.File_Type;
         valid : Boolean := False;
         xmin  : Float;
         xmax  : Float;
         ymin  : Float;
         ymax  : Float;
      end record;
   --
   --  Plot size for scaling
   --
   xSize : constant Float := 1000.0;
   ySize  : constant Float := 1000.0;
   xStart : constant Float := 100.0;
   yStart : constant Float := 100.0;
   borderTop : constant Float := 20.0;
   borderBot : constant Float := 20.0;
   borderLeft  : constant Float := 20.0;
   borderRight : constant Float := 10.0;
   --
   tick_size : constant Integer := 10;
end BBS.Numerical.plot_svg_linear;
