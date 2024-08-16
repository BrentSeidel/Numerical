with Ada.Integer_Text_IO;
with Ada.Float_Text_IO;
package body plot is
   --
   --  Open the named file and start the plot
   --
   procedure start_plot(self : in out plot_record; name : string; xmin, xmax, ymin, ymax : float) is
   begin
      Ada.Text_IO.Create(self.io, Ada.Text_IO.Out_File, name);
      self.xmin := xmin;
      self.xmax := xmax;
      self.ymin := ymin;
      self.ymax := ymax;
      self.valid := True;
      Ada.Text_IO.Put(self.io, "<svg xmlns=""http://www.w3.org/2000/svg""");
      Ada.Text_IO.Put_Line(self.io, " viewbox=""-10 -10 1010 1010"">");
   end;
   --
   --  Draw the plot frame.  The frame is inset by 10 from the viewbox.
   --
   procedure frame(self : in out plot_record; xTicks, yTicks : Natural;
                  xLines , yLines : Boolean) is
      xPos : Integer;
      yPos : Integer;
   begin
      Ada.Text_IO.Put(self.io, "<polyline points=""0,0 1000,0 1000,1000 0,1000 0,0""");
      Ada.Text_IO.Put_Line(self.io, " fill=""none"" stroke=""black"" stroke-width=""2"" />");
      Ada.Text_IO.Put_Line(self.io, "<g stroke=""black"" stroke-width=""1""  font-size=""10pt"">");
      for x in 1 .. xTicks loop
         xPos := x*(1000/xTicks);
         if xLines then
            Ada.Text_IO.Put(self.io, "  <line x1=""");
            Ada.Integer_Text_IO.Put(self.io, xPos, 0);
            Ada.Text_IO.Put(self.io, """ y1=""0"" x2=""");
            Ada.Integer_Text_IO.Put(self.io, xPos, 0);
            Ada.Text_IO.Put_Line(self.io, """ y2=""1000"" />");
         else
            Ada.Text_IO.Put(self.io, "  <line x1=""");
            Ada.Integer_Text_IO.Put(self.io, xPos, 0);
            Ada.Text_IO.Put(self.io, """ y1=""0"" x2=""");
            Ada.Integer_Text_IO.Put(self.io, xPos, 0);
            Ada.Text_IO.Put(self.io, """ y2=""");
            Ada.Integer_Text_IO.Put(self.io, tick_size, 0);
            Ada.Text_IO.Put_line(self.io, """ />");
            Ada.Text_IO.Put(self.io, "  <line x1=""");
            Ada.Integer_Text_IO.Put(self.io, xPos, 0);
            Ada.Text_IO.Put(self.io, """ y1=""");
            Ada.Integer_Text_IO.Put(self.io, 1000 - tick_size, 0);
            Ada.Text_IO.Put(self.io, """ x2=""");
            Ada.Integer_Text_IO.Put(self.io, xPos, 0);
            Ada.Text_IO.Put_Line(self.io, """ y2=""1000"" />");
         end if;
         Ada.Text_IO.Put(self.io, "  <text x=""");
         Ada.Integer_Text_IO.Put(self.io, xPos - 10, 0);
         Ada.Text_IO.Put(self.io, """ y=""1012"">");
         Ada.Float_Text_IO.Put(self.io, self.xMin + float(x)*(self.xMax - self.xMin)/float(xTicks), 2, 2, 0);
         Ada.Text_IO.Put_Line(self.io, "</text>");
      end loop;
      for y in 1 .. yTicks loop
         yPos := 1000 - y*(1000/yTicks);
         if yLines then
            Ada.Text_IO.Put(self.io, "  <line x1=""0"" y1=""");
            Ada.Integer_Text_IO.Put(self.io, yPos, 0);
            Ada.Text_IO.Put(self.io, """ x2=""1000"" y2=""");
            Ada.Integer_Text_IO.Put(self.io, yPos, 0);
            Ada.Text_IO.Put_Line(self.io, """ />");
         else
            Ada.Text_IO.Put(self.io, "  <line x1=""0"" y1=""");
            Ada.Integer_Text_IO.Put(self.io, yPos, 0);
            Ada.Text_IO.Put(self.io, """ x2=""");
            Ada.Integer_Text_IO.Put(self.io, tick_size, 0);
            Ada.Text_IO.Put(self.io, """ y2=""");
            Ada.Integer_Text_IO.Put(self.io, yPos, 0);
            Ada.Text_IO.Put_Line(self.io, """ />");
            Ada.Text_IO.Put(self.io, "  <line x1=""");
            Ada.Integer_Text_IO.Put(self.io, 1000 - tick_size, 0);
            Ada.Text_IO.Put(self.io, """ y1=""");
            Ada.Integer_Text_IO.Put(self.io, yPos, 0);
            Ada.Text_IO.Put(self.io, """ x2=""1000"" y2=""");
            Ada.Integer_Text_IO.Put(self.io, yPos, 0);
            Ada.Text_IO.Put_Line(self.io, """ />");
         end if;
         Ada.Text_IO.Put(self.io, "  <text x=""-5"" y=""");
         Ada.Integer_Text_IO.Put(self.io, yPos + 10, 0);
         Ada.Text_IO.Put(self.io, """ transform=""rotate(-90, -5, ");
         Ada.Integer_Text_IO.Put(self.io, yPos + 10, 0);
         Ada.Text_IO.Put(self.io, ")"">");
         Ada.Float_Text_IO.Put(self.io, self.yMin + float(y)*(self.yMax - self.yMin)/float(yTicks), 2, 2, 0);
         Ada.Text_IO.Put_Line(self.io, "</text>");
      end loop;
      Ada.Text_IO.Put_Line(self.io, "</g>");
   end;
   --
   --  Set Axis label
   --
   procedure label(self : in out plot_record; xLabel, yLabel : String) is
   begin
      Ada.Text_IO.Put(self.io, "<text x=""50"" y=""1025"" stroke=""black"" font-size=""12pt"">");
      Ada.Text_IO.Put_Line(self.io, xLabel & "</text>");
      Ada.Text_IO.Put(self.io, "<text x=""-25"" y=""950"" stroke=""black"" font-size=""12pt"" transform=""rotate(-90, -25, 950)"" >");
      Ada.Text_IO.Put_Line(self.io, yLabel & "</text>");
   end;
   --
   --  Set title
   --
   procedure title(self : in out plot_record; title : String) is
   begin
      Ada.Text_IO.Put(self.io, "<text x=""50"" y=""-10"" stroke=""black"" font-size=""14pt"">");
      Ada.Text_IO.Put_Line(self.io, title & "</text>");
   end;
   --
   --  Plot a set of points
   --
   procedure draw(self : in out plot_record; color : String; line : Boolean;
                  points : point_list) is
      p : point;
      xLast : Float;
      yLast : Float;
      xNext : Float;
      yNext : Float;
   begin
      if line then
         p := points(points'First);
         xLast := (p.x - self.xMin)*width/(self.xMax - self.xMin);
         yLast := height - (p.y - self.yMin)*height/(self.yMax - self.yMin);
         Ada.Text_IO.Put_Line(self.io, "<g stroke=""" & color & """ stroke-width=""1"">");
         for x in points'First + 1 .. points'Last loop
            p := points(x);
            xNext := (p.x - self.xMin)*width/(self.xMax - self.xMin);
            yNext := height - (p.y - self.yMin)*height/(self.yMax - self.yMin);
            Ada.Text_IO.Put(self.io, "  <line x1=""");
            Ada.Integer_Text_IO.Put(self.io, Integer(xLast), 0);
            Ada.Text_IO.Put(self.io, """ y1=""");
            Ada.Integer_Text_IO.Put(self.io, Integer(yLast), 0);
            Ada.Text_IO.Put(self.io, """ x2=""");
            Ada.Integer_Text_IO.Put(self.io, Integer(xNext), 0);
            Ada.Text_IO.Put(self.io, """ y2=""");
            Ada.Integer_Text_IO.Put(self.io, Integer(yNext), 0);
            Ada.Text_IO.Put_Line(self.io, """ />");
            xLast := xNext;
            yLast := yNext;
         end loop;
      else
         Ada.Text_IO.Put_Line(self.io, "<g fill=""" & color & """>");
         for p of points loop
            Ada.Text_IO.Put(self.io, "  <circle cx=""");
            Ada.Float_Text_IO.Put(self.io, (p.x - self.xMin)*width/(self.xMax - self.xMin), 0);
            Ada.Text_IO.Put(self.io, """ cy=""");
            Ada.Float_Text_IO.Put(self.io, height - (p.y - self.yMin)*height/(self.yMax - self.yMin), 0);
            Ada.Text_IO.Put_line(self.io, """ r=""2"" />");
         end loop;
      end if;
      Ada.Text_IO.Put_Line(self.io, "</g>");
   end;
   --
   --  Close the plot
   --
   procedure end_plot(self : in out plot_record) is
   begin
      if self.valid then
         Ada.Text_IO.Put_Line(self.io, "</svg>");
         Ada.Text_IO.Close(self.io);
      end if;
   end;
end plot;
