with Ada.Integer_Text_IO;
with Ada.Float_Text_IO;
package body BBS.Numerical.plot_svg_linear is
   --
   --  Open the named file and start the plot
   --
   procedure start_plot(self : in out linear_svg_plot_record; name : string;
                        xmin, xmax, ymin, ymax : float) is
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
   procedure frame(self : in out linear_svg_plot_record; xTicks, yTicks : Natural;
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
   procedure label(self : in out linear_svg_plot_record; xLabel, yLabel : String) is
   xPos : constant Integer := Integer(xStart) - 25;
   yPos : constant Integer := Integer(yStart + ySize) - 50;
   begin
      if not self.valid then
         return;
      end if;
      Ada.Text_IO.Put(self.io, "<text x=""");
      Ada.Integer_Text_IO.Put(self.io, Integer(xStart) + 50, 0);
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
   procedure title(self : in out linear_svg_plot_record; title : String) is
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
   --  Plotting helper functions
   --
   function xTranslate(self : linear_svg_plot_record; x : Float) return Float is
   begin
      return xStart + (x - self.xMin)*xSize/(self.xMax - self.xMin);
   end;
   --
   function yTranslate(self : linear_svg_plot_record; y : Float) return Float is
   begin
      return yStart +  ySize - (y - self.yMin)*ySize/(self.yMax - self.yMin);
   end;
   --
   --  Plot lines
   --
   procedure draw_line(self : in out linear_svg_plot_record;
                     points : BBS.Numerical.plot.point_list; color : String) is
   begin
      if not self.valid then
         return;
      end if;
      Ada.Text_IO.Put(self.io, "<polyline points=""");
      for p of points loop
         Ada.Integer_Text_IO.Put(self.io, Integer(self.xTranslate(p.x)), 0);
         Ada.Text_IO.Put(self.io, ",");
         Ada.Integer_Text_IO.Put(self.io, Integer(self.yTranslate( p.y)), 0);
         Ada.Text_IO.Put(self.io, " ");
      end loop;
      Ada.Text_IO.Put_Line(self.io, """ fill=""none"" stroke=""" & color & """ stroke-width=""1"" />");
   end;
   --
   --  Plot point(s)
   --
   procedure draw_point(self : in out linear_svg_plot_record;
                        p : BBS.Numerical.plot.point; size : Float;
                        color : String) is
   begin
      if not self.valid then
         return;
      end if;
      Ada.Text_IO.Put(self.io, "<circle cx=""");
      Ada.Float_Text_IO.Put(self.io, self.xTranslate(p.x), 0);
      Ada.Text_IO.Put(self.io, """ cy=""");
      Ada.Float_Text_IO.Put(self.io, self.yTranslate(p.y), 0);
      Ada.Text_IO.Put(self.io, """ r=""");
      Ada.Integer_Text_IO.Put(self.io, Integer(size), 0);
      Ada.Text_IO.Put_Line(self.io, """ fill=""" & color & """ />");
   end;
   --
   procedure draw_point(self : in out linear_svg_plot_record;
                        points : BBS.Numerical.plot.point_list; size : Float;
                        color : String) is
   begin
      if not self.valid then
         return;
      end if;
      Ada.Text_IO.Put_Line(self.io, "<g fill=""" & color & """>");
      for p of points loop
         Ada.Text_IO.Put(self.io, "  <circle cx=""");
         Ada.Float_Text_IO.Put(self.io, self.xTranslate(p.x), 0);
         Ada.Text_IO.Put(self.io, """ cy=""");
         Ada.Float_Text_IO.Put(self.io, self.yTranslate(p.y), 0);
         Ada.Text_IO.Put_line(self.io, """ r="" />");
         Ada.Integer_Text_IO.Put(self.io, Integer(size), 0);
         Ada.Text_IO.Put_Line(self.io, " />");
      end loop;
      Ada.Text_IO.Put_Line(self.io, "</g>");
   end;
   --
   --  Draw text at a point
   --
   procedure draw_text(self : in out linear_svg_plot_record;
                        p : BBS.Numerical.plot.point; color, text : String) is
   begin
      if not self.valid then
         return;
      end if;
      Ada.Text_IO.Put(self.io, "<text x=""");
      Ada.Integer_Text_IO.Put(self.io, Integer(self.xTranslate(p.x)));
      Ada.Text_IO.Put(self.io, """ y=""");
      Ada.Integer_Text_IO.Put(self.io, Integer(self.yTranslate(p.y)));
      Ada.Text_IO.Put_Line(self.io, """ stroke=""" & color & """ font-size=""10pt"">" & text & "</text>");
   end;
   --
   --  Glyph helper functions
   --
   procedure draw_glyph_plus(self : in out linear_svg_plot_record;
                              xPos, yPos, size : Integer) is
   begin
      Ada.Text_IO.Put(self.io, "  <line x1=""");
      Ada.Integer_Text_IO.Put(self.io, xPos - size, 0);
      Ada.Text_IO.Put(self.io, """ y1=""");
      Ada.Integer_Text_IO.Put(self.io, yPos, 0);
      Ada.Text_IO.Put(self.io, """ x2=""");
      Ada.Integer_Text_IO.Put(self.io, xPos + size, 0);
      Ada.Text_IO.Put(self.io, """ y2=""");
      Ada.Integer_Text_IO.Put(self.io, yPos, 0);
      Ada.Text_IO.Put_Line(self.io, """ />");
      Ada.Text_IO.Put(self.io, "  <line x1=""");
      Ada.Integer_Text_IO.Put(self.io, xPos, 0);
      Ada.Text_IO.Put(self.io, """ y1=""");
      Ada.Integer_Text_IO.Put(self.io, yPos - size, 0);
      Ada.Text_IO.Put(self.io, """ x2=""");
      Ada.Integer_Text_IO.Put(self.io, xPos, 0);
      Ada.Text_IO.Put(self.io, """ y2=""");
      Ada.Integer_Text_IO.Put(self.io, yPos + size, 0);
      Ada.Text_IO.Put_Line(self.io, """ />");
   end;
   --
   procedure draw_glyph_X(self : in out linear_svg_plot_record;
                              xPos, yPos, size : Integer) is
   begin
      Ada.Text_IO.Put(self.io, "  <line x1=""");
      Ada.Integer_Text_IO.Put(self.io, xPos - size, 0);
      Ada.Text_IO.Put(self.io, """ y1=""");
      Ada.Integer_Text_IO.Put(self.io, yPos - size, 0);
      Ada.Text_IO.Put(self.io, """ x2=""");
      Ada.Integer_Text_IO.Put(self.io, xPos + size, 0);
      Ada.Text_IO.Put(self.io, """ y2=""");
      Ada.Integer_Text_IO.Put(self.io, yPos + size, 0);
      Ada.Text_IO.Put_Line(self.io, """ />");
      Ada.Text_IO.Put(self.io, "  <line x1=""");
      Ada.Integer_Text_IO.Put(self.io, xPos + size, 0);
      Ada.Text_IO.Put(self.io, """ y1=""");
      Ada.Integer_Text_IO.Put(self.io, yPos - size, 0);
      Ada.Text_IO.Put(self.io, """ x2=""");
      Ada.Integer_Text_IO.Put(self.io, xPos - size, 0);
      Ada.Text_IO.Put(self.io, """ y2=""");
      Ada.Integer_Text_IO.Put(self.io, yPos + size, 0);
      Ada.Text_IO.Put_Line(self.io, """ />");
   end;
   --
   procedure draw_glyph_asterisk(self : in out linear_svg_plot_record;
                              xPos, yPos, size : Integer) is
   begin
      self.draw_glyph_plus(xPos, yPos, size);
      self.draw_glyph_X(xPos, yPos, size);
   end;
   --
   procedure draw_glyph_box(self : in out linear_svg_plot_record;
                           xPos, yPos, size : Integer) is
   begin
      Ada.Text_IO.Put(self.io, "  <polyline points=""");
      Ada.Integer_Text_IO.Put(self.io, xPos + size, 0);
      Ada.Text_IO.Put(self.io, ",");
      Ada.Integer_Text_IO.Put(self.io, yPos + size, 0);
      Ada.Text_IO.Put(self.io, " ");
      Ada.Integer_Text_IO.Put(self.io, xPos - size, 0);
      Ada.Text_IO.Put(self.io, ",");
      Ada.Integer_Text_IO.Put(self.io, yPos + size, 0);
      Ada.Text_IO.Put(self.io, " ");
      Ada.Integer_Text_IO.Put(self.io, xPos - size, 0);
      Ada.Text_IO.Put(self.io, ",");
      Ada.Integer_Text_IO.Put(self.io, yPos - size, 0);
      Ada.Text_IO.Put(self.io, " ");
      Ada.Integer_Text_IO.Put(self.io, xPos + size, 0);
      Ada.Text_IO.Put(self.io, ",");
      Ada.Integer_Text_IO.Put(self.io, yPos - size, 0);
      Ada.Text_IO.Put(self.io, " ");
      Ada.Integer_Text_IO.Put(self.io, xPos + size, 0);
      Ada.Text_IO.Put(self.io, ",");
      Ada.Integer_Text_IO.Put(self.io, yPos + size, 0);
      Ada.Text_IO.Put(self.io, """ fill=""none"" />");
   end;
   --
   procedure draw_glyph_diamond(self : in out linear_svg_plot_record;
                              xPos, yPos, size : Integer) is
   begin
      Ada.Text_IO.Put(self.io, "  <polyline points=""");
      Ada.Integer_Text_IO.Put(self.io, xPos, 0);
      Ada.Text_IO.Put(self.io, ",");
      Ada.Integer_Text_IO.Put(self.io, yPos + size, 0);
      Ada.Text_IO.Put(self.io, " ");
      Ada.Integer_Text_IO.Put(self.io, xPos - size, 0);
      Ada.Text_IO.Put(self.io, ",");
      Ada.Integer_Text_IO.Put(self.io, yPos, 0);
      Ada.Text_IO.Put(self.io, " ");
      Ada.Integer_Text_IO.Put(self.io, xPos, 0);
      Ada.Text_IO.Put(self.io, ",");
      Ada.Integer_Text_IO.Put(self.io, yPos - size, 0);
      Ada.Text_IO.Put(self.io, " ");
      Ada.Integer_Text_IO.Put(self.io, xPos + size, 0);
      Ada.Text_IO.Put(self.io, ",");
      Ada.Integer_Text_IO.Put(self.io, yPos, 0);
      Ada.Text_IO.Put(self.io, " ");
      Ada.Integer_Text_IO.Put(self.io, xPos, 0);
      Ada.Text_IO.Put(self.io, ",");
      Ada.Integer_Text_IO.Put(self.io, yPos + size, 0);
      Ada.Text_IO.Put(self.io, """ fill=""none"" />");
   end;
   --
   procedure draw_glyph_octagon(self : in out linear_svg_plot_record;
                              xPos, yPos, size : Integer) is
   begin
      Ada.Text_IO.Put(self.io, "  <polyline points=""");
      Ada.Integer_Text_IO.Put(self.io, xPos - size/2, 0);
      Ada.Text_IO.Put(self.io, ",");
      Ada.Integer_Text_IO.Put(self.io, yPos + size, 0);
      Ada.Text_IO.Put(self.io, " ");
      Ada.Integer_Text_IO.Put(self.io, xPos + size/2, 0);
      Ada.Text_IO.Put(self.io, ",");
      Ada.Integer_Text_IO.Put(self.io, yPos + size, 0);
      Ada.Text_IO.Put(self.io, " ");
      Ada.Integer_Text_IO.Put(self.io, xPos + size, 0);
      Ada.Text_IO.Put(self.io, ",");
      Ada.Integer_Text_IO.Put(self.io, yPos + size/2, 0);
      Ada.Text_IO.Put(self.io, " ");
      Ada.Integer_Text_IO.Put(self.io, xPos + size, 0);
      Ada.Text_IO.Put(self.io, ",");
      Ada.Integer_Text_IO.Put(self.io, yPos - size/2, 0);
      Ada.Text_IO.Put(self.io, " ");
      Ada.Integer_Text_IO.Put(self.io, xPos + size/2, 0);
      Ada.Text_IO.Put(self.io, ",");
      Ada.Integer_Text_IO.Put(self.io, yPos - size, 0);
      Ada.Text_IO.Put(self.io, " ");
      Ada.Integer_Text_IO.Put(self.io, xPos - size/2, 0);
      Ada.Text_IO.Put(self.io, ",");
      Ada.Integer_Text_IO.Put(self.io, yPos - size, 0);
      Ada.Text_IO.Put(self.io, " ");
      Ada.Integer_Text_IO.Put(self.io, xPos - size, 0);
      Ada.Text_IO.Put(self.io, ",");
      Ada.Integer_Text_IO.Put(self.io, yPos - size/2, 0);
      Ada.Text_IO.Put(self.io, " ");
      Ada.Integer_Text_IO.Put(self.io, xPos - size, 0);
      Ada.Text_IO.Put(self.io, ",");
      Ada.Integer_Text_IO.Put(self.io, yPos + size/2, 0);
      Ada.Text_IO.Put(self.io, " ");
      Ada.Integer_Text_IO.Put(self.io, xPos - size/2, 0);
      Ada.Text_IO.Put(self.io, ",");
      Ada.Integer_Text_IO.Put(self.io, yPos + size, 0);
      Ada.Text_IO.Put(self.io, """ fill=""none"" />");
   end;
   --
   procedure select_glyph(self : in out linear_svg_plot_record;
                        g : BBS.Numerical.plot.glyph;
                        xPos, yPos, size : Integer) is
   begin
      case g is
         when BBS.Numerical.plot.glyph_plus =>
            self.draw_glyph_plus(xPos, yPos, size);
         when BBS.Numerical.plot.glyph_X =>
            self.draw_glyph_X(xPos, yPos, size);
         when BBS.Numerical.plot.glyph_asterisk =>
            self.draw_glyph_asterisk(xPos, yPos, size);
         when BBS.Numerical.plot.glyph_box =>
            self.draw_glyph_box(xPos, yPos, size);
         when BBS.Numerical.plot.glyph_diamond =>
            self.draw_glyph_diamond(xPos, yPos, size);
         when BBS.Numerical.plot.glyph_octagon =>
            self.draw_glyph_octagon(xPos, yPos, size);
      end case;
   end;
   --
   procedure draw_err(self : in out linear_svg_plot_record;
                       xPos, yPos, size, err : Integer) is
   begin
      Ada.Text_IO.Put(self.io, "  <line x1=""");
      Ada.Integer_Text_IO.Put(self.io, xPos - size, 0);
      Ada.Text_IO.Put(self.io, """ y1=""");
      Ada.Integer_Text_IO.Put(self.io, yPos - err, 0);
      Ada.Text_IO.Put(self.io, """ x2=""");
      Ada.Integer_Text_IO.Put(self.io, xPos + size, 0);
      Ada.Text_IO.Put(self.io, """ y2=""");
      Ada.Integer_Text_IO.Put(self.io, yPos - err, 0);
      Ada.Text_IO.Put_Line(self.io, """ />");
      Ada.Text_IO.Put(self.io, "  <line x1=""");
      Ada.Integer_Text_IO.Put(self.io, xPos, 0);
      Ada.Text_IO.Put(self.io, """ y1=""");
      Ada.Integer_Text_IO.Put(self.io, yPos - err, 0);
      Ada.Text_IO.Put(self.io, """ x2=""");
      Ada.Integer_Text_IO.Put(self.io, xPos, 0);
      Ada.Text_IO.Put(self.io, """ y2=""");
      Ada.Integer_Text_IO.Put(self.io, yPos + err, 0);
      Ada.Text_IO.Put_Line(self.io, """ />");
      Ada.Text_IO.Put(self.io, "  <line x1=""");
      Ada.Integer_Text_IO.Put(self.io, xPos - size, 0);
      Ada.Text_IO.Put(self.io, """ y1=""");
      Ada.Integer_Text_IO.Put(self.io, yPos + err, 0);
      Ada.Text_IO.Put(self.io, """ x2=""");
      Ada.Integer_Text_IO.Put(self.io, xPos + size, 0);
      Ada.Text_IO.Put(self.io, """ y2=""");
      Ada.Integer_Text_IO.Put(self.io, yPos + err, 0);
      Ada.Text_IO.Put_Line(self.io, """ />");
   end;
   --
   --  Draw a glyph at a point
   --
   procedure draw_glyph(self : in out linear_svg_plot_record;
                        p : BBS.Numerical.plot.point;
                        g : BBS.Numerical.plot.glyph; color : String) is
      size : constant Integer := 10;
   begin
      if not self.valid then
         return;
      end if;
      Ada.Text_IO.Put_Line(self.io, "<g stroke=""" & color & """ stroke-width=""1"">");
      self.select_glyph(g, Integer(self.xTranslate(p.x)), Integer(self.yTranslate(p.y)), size);
      Ada.Text_IO.Put_Line(self.io, "</g>");
   end;
   --
   procedure draw_glyph(self : in out linear_svg_plot_record;
                        points : BBS.Numerical.plot.point_list;
                        g : BBS.Numerical.plot.glyph; color : String) is
      size : constant Integer := 10;
   begin
      if not self.valid then
         return;
      end if;
      Ada.Text_IO.Put_Line(self.io, "<g stroke=""" & color & """ stroke-width=""1"">");
      for p of points loop
         self.select_glyph(g, Integer(self.xTranslate(p.x)), Integer(self.yTranslate(p.y)), size);
      end loop;
      Ada.Text_IO.Put_Line(self.io, "</g>");
   end;
   --
   --  Draw glyphs with error bars
   --
   procedure draw_glyph(self : in out linear_svg_plot_record;
                        p : BBS.Numerical.plot.point_err;
                        g : BBS.Numerical.plot.glyph; color : String) is
      xPos : constant Integer := Integer(self.xTranslate(p.x));
      yPos : constant Integer := Integer(self.yTranslate(p.y));
      err  : constant Integer := Integer(p.e*ySize/(self.yMax - self.yMin));
      size : constant Integer := 10;
   begin
      if not self.valid then
         return;
      end if;
      Ada.Text_IO.Put_Line(self.io, "<g stroke=""" & color & """ stroke-width=""1"">");
      self.select_glyph(g, xPos, yPos, size);
      self.draw_err(xPos, yPos, size, err);
      Ada.Text_IO.Put_Line(self.io, "</g>");
   end;
   --
   procedure draw_glyph(self : in out linear_svg_plot_record;
                        points : BBS.Numerical.plot.point_err_list;
                        g : BBS.Numerical.plot.glyph; color : String) is
      xPos : Integer;
      yPos : Integer;
      err  : Integer;
      size : constant Integer := 10;
   begin
      if not self.valid then
         return;
      end if;
      Ada.Text_IO.Put_Line(self.io, "<g stroke=""" & color & """ stroke-width=""1"">");
      for p of points loop
         xPos := Integer(self.xTranslate(p.x));
         yPos := Integer(self.yTranslate(p.y));
         err  := Integer(p.e*ySize/(self.yMax - self.yMin));
         self.select_glyph(g, xPos, yPos, size);
         self.draw_err(xPos, yPos, size, err);
      end loop;
      Ada.Text_IO.Put_Line(self.io, "</g>");
   end;
   --
   --  Close the plot
   --
   procedure end_plot(self : in out linear_svg_plot_record) is
   begin
      if self.valid then
         Ada.Text_IO.Put_Line(self.io, "</svg>");
         Ada.Text_IO.Close(self.io);
      end if;
      self.valid := False;
   end;
end BBS.Numerical.plot_svg_linear;
