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
      Ada.Text_IO.Put(self.io, " viewbox=""");
      Ada.Integer_Text_IO.Put(self.io, Integer(xStart - borderLeft), 0);
      Ada.Text_IO.Put(self.io, " ");
      Ada.Integer_Text_IO.Put(self.io, Integer(yStart - borderTop), 0);
      Ada.Text_IO.Put(self.io, " ");
      Ada.Integer_Text_IO.Put(self.io, Integer(xStart + xSize + borderRight), 0);
      Ada.Text_IO.Put(self.io, " ");
      Ada.Integer_Text_IO.Put(self.io, Integer(yStart + ySize + borderBot), 0);
      Ada.Text_IO.Put_Line(self.io, """>");
   end;
   --
   --  Draw the plot frame.
   --
   procedure frame(self : in out plot_record; xTicks, yTicks : Natural;
                  xLines , yLines : Boolean) is
      xPos : Integer;
      yPos : Integer;
   begin
      if not self.valid then
         return;
      end if;
      Ada.Text_IO.Put(self.io, "<polyline points=""");
      Ada.Integer_Text_IO.Put(self.io, Integer(xStart), 0);
      Ada.Text_IO.Put(self.io, ",");
      Ada.Integer_Text_IO.Put(self.io, Integer(yStart), 0);
      Ada.Text_IO.Put(self.io, " ");
      Ada.Integer_Text_IO.Put(self.io, Integer(xStart + xSize), 0);
      Ada.Text_IO.Put(self.io, ",");
      Ada.Integer_Text_IO.Put(self.io, Integer(yStart), 0);
      Ada.Text_IO.Put(self.io, " ");
      Ada.Integer_Text_IO.Put(self.io, Integer(xStart + xSize), 0);
      Ada.Text_IO.Put(self.io, ",");
      Ada.Integer_Text_IO.Put(self.io, Integer(yStart + ySize), 0);
      Ada.Text_IO.Put(self.io, " ");
      Ada.Integer_Text_IO.Put(self.io, Integer(xStart), 0);
      Ada.Text_IO.Put(self.io, ",");
      Ada.Integer_Text_IO.Put(self.io, Integer(yStart + ySize), 0);
      Ada.Text_IO.Put(self.io, " ");
      Ada.Integer_Text_IO.Put(self.io, Integer(xStart), 0);
      Ada.Text_IO.Put(self.io, ",");
      Ada.Integer_Text_IO.Put(self.io, Integer(yStart), 0);
      Ada.Text_IO.Put_Line(self.io, """ fill=""none"" stroke=""black"" stroke-width=""2"" />");
      Ada.Text_IO.Put_Line(self.io, "<g stroke=""black"" stroke-width=""1""  font-size=""10pt"">");
      --
      --  X axis ticks and numbers
      for x in 1 .. xTicks - 1 loop
         xPos := Integer(xStart) + x*(Integer(xSize)/xTicks);
         if xLines then
            Ada.Text_IO.Put(self.io, "  <line x1=""");
            Ada.Integer_Text_IO.Put(self.io, xPos, 0);
            Ada.Text_IO.Put(self.io, """ y1=""");
            Ada.Integer_Text_IO.Put(self.io, Integer(yStart), 0);
            Ada.Text_IO.Put(self.io, """ x2=""");
            Ada.Integer_Text_IO.Put(self.io, xPos, 0);
            Ada.Text_IO.Put_Line(self.io, """ y2=""");
            Ada.Integer_Text_IO.Put(self.io, Integer(yStart + ySize), 0);
            Ada.Text_IO.Put_Line(self.io, """ />");
         else
            Ada.Text_IO.Put(self.io, "  <line x1=""");
            Ada.Integer_Text_IO.Put(self.io, xPos, 0);
            Ada.Text_IO.Put(self.io, """ y1=""");
            Ada.Integer_Text_IO.Put(self.io, Integer(yStart), 0);
            Ada.Text_IO.Put(self.io, """ x2=""");
            Ada.Integer_Text_IO.Put(self.io, xPos, 0);
            Ada.Text_IO.Put(self.io, """ y2=""");
            Ada.Integer_Text_IO.Put(self.io, Integer(yStart) + tick_size, 0);
            Ada.Text_IO.Put_Line(self.io, """ />");
            Ada.Text_IO.Put(self.io, "  <line x1=""");
            Ada.Integer_Text_IO.Put(self.io, xPos, 0);
            Ada.Text_IO.Put(self.io, """ y1=""");
            Ada.Integer_Text_IO.Put(self.io, Integer(yStart + ySize) - tick_size, 0);
            Ada.Text_IO.Put(self.io, """ x2=""");
            Ada.Integer_Text_IO.Put(self.io, xPos, 0);
            Ada.Text_IO.Put(self.io, """ y2=""");
            Ada.Integer_Text_IO.Put(self.io, Integer(yStart + ySize), 0);
            Ada.Text_IO.Put_Line(self.io, """ />");
         end if;
         Ada.Text_IO.Put(self.io, "  <text x=""");
         Ada.Integer_Text_IO.Put(self.io, xPos - 10, 0);
         Ada.Text_IO.Put(self.io, """ y=""");
         Ada.Integer_Text_IO.Put(self.io, Integer(yStart + ySize) + 12, 0);
         Ada.Text_IO.Put(self.io, """>");
         Ada.Float_Text_IO.Put(self.io, self.xMin + float(x)*(self.xMax - self.xMin)/float(xTicks), 2, 2, 0);
         Ada.Text_IO.Put_Line(self.io, "</text>");
      end loop;
      --
      --  Y axis ticks and numbers
      --
      for y in 1 .. yTicks - 1 loop
         yPos := Integer(yStart + ySize) - y*(Integer(ySize)/yTicks);
         if yLines then
            Ada.Text_IO.Put(self.io, "  <line x1=""");
            Ada.Integer_Text_IO.Put(self.io, Integer(xStart), 0);
            Ada.Text_IO.Put(self.io, """ y1=""");
            Ada.Integer_Text_IO.Put(self.io, yPos, 0);
            Ada.Text_IO.Put(self.io, """ x2=""");
            Ada.Integer_Text_IO.Put(self.io, Integer(xStart + xSize), 0);
            Ada.Text_IO.Put(self.io, """ y2=""");
            Ada.Integer_Text_IO.Put(self.io, yPos, 0);
            Ada.Text_IO.Put_Line(self.io, """ />");
         else
            Ada.Text_IO.Put(self.io, "  <line x1=""");
            Ada.Integer_Text_IO.Put(self.io, Integer(xStart), 0);
            Ada.Text_IO.Put(self.io, """ y1=""");
            Ada.Integer_Text_IO.Put(self.io, yPos, 0);
            Ada.Text_IO.Put(self.io, """ x2=""");
            Ada.Integer_Text_IO.Put(self.io, Integer(xStart) + tick_size, 0);
            Ada.Text_IO.Put(self.io, """ y2=""");
            Ada.Integer_Text_IO.Put(self.io, yPos, 0);
            Ada.Text_IO.Put_Line(self.io, """ />");
            Ada.Text_IO.Put(self.io, "  <line x1=""");
            Ada.Integer_Text_IO.Put(self.io, Integer(xStart + xSize) - tick_size, 0);
            Ada.Text_IO.Put(self.io, """ y1=""");
            Ada.Integer_Text_IO.Put(self.io, yPos, 0);
            Ada.Text_IO.Put(self.io, """ x2=""");
            Ada.Integer_Text_IO.Put(self.io, Integer(xStart + xSize), 0);
            Ada.Text_IO.Put(self.io, """ y2=""");
            Ada.Integer_Text_IO.Put(self.io, yPos, 0);
            Ada.Text_IO.Put_Line(self.io, """ />");
         end if;
         Ada.Text_IO.Put(self.io, "  <text x=""");
         Ada.Integer_Text_IO.Put(self.io, Integer(xStart) - 5, 0);
         Ada.Text_IO.Put(self.io, """ y=""");
         Ada.Integer_Text_IO.Put(self.io, yPos + 10, 0);
         Ada.Text_IO.Put(self.io, """ transform=""rotate(-90, ");
         Ada.Integer_Text_IO.Put(self.io, Integer(yStart) - 5, 0);
         Ada.Text_IO.Put(self.io, ", ");
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
   xPos : constant Integer := Integer(xStart) - 25;
   yPos : constant Integer := Integer(yStart + ySize) - 50;
   begin
      if not self.valid then
         return;
      end if;
      Ada.Text_IO.Put(self.io, "<text x=""");
      Ada.Integer_Text_IO.Put(self.io, Integer(xStart) + 50);
      Ada.Text_IO.Put(self.io, """ y=""");
      Ada.Integer_Text_IO.Put(self.io, Integer(yStart + ySize) + 25, 0);
      Ada.Text_IO.Put(self.io, """ stroke=""black"" font-size=""12pt"">");
      Ada.Text_IO.Put_Line(self.io, xLabel & "</text>");
      Ada.Text_IO.Put(self.io, "<text x=""");
      Ada.Integer_Text_IO.Put(self.io, xPos, 0);
      Ada.Text_IO.Put(self.io, """ y=""");
      Ada.Integer_Text_IO.Put(self.io, yPos, 0);
      Ada.Text_IO.Put(self.io, """ stroke=""black"" font-size=""12pt"" transform=""rotate(-90, ");
      Ada.Integer_Text_IO.Put(self.io, xPos, 0);
      Ada.Text_IO.Put(self.io, ", ");
      Ada.Integer_Text_IO.Put(self.io, yPos, 0);
      Ada.Text_IO.Put_Line(self.io, ")"" >" & yLabel & "</text>");
   end;
   --
   --  Set title
   --
   procedure title(self : in out plot_record; title : String) is
   begin
      if not self.valid then
         return;
      end if;
      Ada.Text_IO.Put(self.io, "<text x=""");
      Ada.Integer_Text_IO.Put(self.io, Integer(xStart) + 50, 0);
      Ada.Text_IO.Put(self.io, """ y=""");
      Ada.Integer_Text_IO.Put(self.io, Integer(yStart) - 10, 0);
      Ada.Text_IO.Put_Line(self.io, """ stroke=""black"" font-size=""14pt"">" & title & "</text>");
   end;
   --
   --  Plot a set of points
   --
   function xTranslate(self : plot_record; x : Float) return Float is
   begin
      return xStart + (x - self.xMin)*xSize/(self.xMax - self.xMin);
   end;
   function yTranslate(self : plot_record; y : Float) return Float is
   begin
      return yStart +  ySize - (y - self.yMin)*ySize/(self.yMax - self.yMin);
   end;
   procedure draw(self : in out plot_record; color : String; line : Boolean;
                  points : point_list) is
      p : point;
      xLast : Float;
      yLast : Float;
      xNext : Float;
      yNext : Float;
   begin
      if not self.valid then
         return;
      end if;
      if line then
         p := points(points'First);
         xLast := self.xTranslate(p.x);
         yLast := self.yTranslate(p.y);
         Ada.Text_IO.Put_Line(self.io, "<g stroke=""" & color & """ stroke-width=""1"">");
         for x in points'First + 1 .. points'Last loop
            p := points(x);
            xNext := self.xTranslate(p.x);
            yNext := self.yTranslate( p.y);
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
            Ada.Float_Text_IO.Put(self.io, self.xTranslate(p.x), 0);
            Ada.Text_IO.Put(self.io, """ cy=""");
            Ada.Float_Text_IO.Put(self.io, self.yTranslate(p.y), 0);
            Ada.Text_IO.Put_line(self.io, """ r=""2"" />");
         end loop;
      end if;
      Ada.Text_IO.Put_Line(self.io, "</g>");
   end;
   --
   --  Plot a single point
   --
   procedure draw_point(self : in out plot_record; color : String; p : point) is
   begin
      Ada.Text_IO.Put(self.io, "<circle cx=""");
      Ada.Float_Text_IO.Put(self.io, self.xTranslate(p.x), 0);
      Ada.Text_IO.Put(self.io, """ cy=""");
      Ada.Float_Text_IO.Put(self.io, self.yTranslate(p.y), 0);
      Ada.Text_IO.Put_line(self.io, """ r=""2"" fill=""" & color & """ />");
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
      self.valid := False;
   end;
end plot;
