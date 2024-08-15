--
--  Package for plotting data.  The data is written out to a SVG format
--  file.
with Ada.Text_IO;
package plot is
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
   --  Open the named file and start the plot
   --
   procedure start_plot(self : in out plot_record; name : string; xmin, xmax, ymin, ymax : float);
   --
   --  Draw the plot frame
   --
   procedure frame(self : in out plot_record; xTicks, yTicks : Natural;
                  xLines , yLines : Boolean);
   --
   --  Plot a set of points
   --
   procedure draw(self : in out plot_record; color : String; line : Boolean;
                  points : point_list);
   --
   --  Close the plot
   --
   procedure end_plot(self : in out plot_record);
   --
private
   type plot_record is tagged limited record
      io    : Ada.Text_IO.File_Type;
      valid : Boolean := False;
      xmin  : Float;
      xmax  : Float;
      ymin  : Float;
      ymax  : Float;
   end record;
   --
   --  Display size for scaling
   --
   height : constant Float := 1000.0;
   width  : constant Float := 1000.0;
   tick_size : constant Integer := 10;
end plot;

