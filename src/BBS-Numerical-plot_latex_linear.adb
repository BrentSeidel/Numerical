with Ada.Float_Text_IO;
package body BBS.Numerical.plot_latex_linear is
   --
   --  Plotting helper functions.  Translate x and y graph units into physical
   --  units for plotting.
   --
   function xTranslate(self : linear_latex_plot_record; x : Float) return Float is
   begin
      return xStart + (x - self.xMin)*xSize/(self.xMax - self.xMin);
   end;
   --
   function yTranslate(self : linear_latex_plot_record; y : Float) return Float is
   begin
      return yStart + (y - self.yMin)*ySize/(self.yMax - self.yMin);
   end;
   --
   --  Open the named file and start the plot
   --
   procedure start_plot(self : in out linear_latex_plot_record; name : string;
                        xmin, xmax, ymin, ymax : float) is
   begin
      Ada.Text_IO.Create(self.io, Ada.Text_IO.Out_File, name);
      self.xmin := xmin;
      self.xmax := xmax;
      self.ymin := ymin;
      self.ymax := ymax;
      self.valid := True;
      Ada.Text_IO.Put_Line(self.io, "\begin{tikzpicture}");
   end;
   --
   --  Draw the plot frame.
   --
   procedure frame(self : in out linear_latex_plot_record; xTicks, yTicks : Natural;
                  xLines , yLines : Boolean) is
      xPos : Float;
      yPos : Float;
   begin
      if not self.valid then
         return;
      end if;
      Ada.Text_IO.Put(self.io, "\draw (" & Float'Image(xStart) &  "," & Float'Image(yStart));
      Ada.Text_IO.Put(self.io, ") rectangle (" & Float'Image(xStart + xSize) & ",");
      Ada.Text_IO.Put_line(self.io, Float'Image(yStart + ySize) & ");");
      --
      --  X axis ticks and numbers
      --
      for x in 1 .. xTicks - 1 loop
         xPos := xStart + Float(x)*xSize/Float(xTicks);
         if xLines then
            Ada.Text_IO.Put(self.io, "\draw (" & Float'Image(xPos) & ",");
            Ada.Text_IO.Put(self.io, Float'Image(yStart) & ") -- (" & Float'Image(xPos));
            Ada.Text_IO.Put_Line(self.io, "," & Float'Image(yStart + ySize) & ");");
         else
            Ada.Text_IO.Put(self.io, "\draw (" & Float'Image(xPos) & ",");
            Ada.Text_IO.Put(self.io, Float'Image(yStart) & ") -- (" & Float'Image(xPos));
            Ada.Text_IO.Put_Line(self.io, "," & Float'Image(yStart + tick_size) & ");");
            Ada.Text_IO.Put(self.io, "\draw (" & Float'Image(xPos) & ",");
            Ada.Text_IO.Put(self.io, Float'Image(yStart + ySize - tick_size) & ") -- (");
            Ada.Text_IO.Put(self.io, Float'Image(xPos) & "," & Float'Image(yStart + ySize));
            Ada.Text_IO.Put_Line(self.io, ");");
         end if;
         Ada.Text_IO.Put(self.io, "\node at (" & Float'Image(xPos) & ",");
         Ada.Text_IO.Put(self.io, Float'Image(yStart - 0.5) & ") {");
         Ada.Float_Text_IO.Put(self.io, self.xMin + float(x)*(self.xMax - self.xMin)/float(xTicks), 2, 2, 0);
         Ada.Text_IO.Put_Line(self.io, "};");
      end loop;
      --
      --  Y axis ticks and numbers
      --
      for y in 1 .. yTicks - 1 loop
         yPos := yStart + Float(y)*ySize/Float(yTicks);
         if yLines then
            Ada.Text_IO.Put(self.io, "\draw (" & Float'Image(xStart) & ",");
            Ada.Text_IO.Put(self.io, Float'Image(yPos) & ") -- (" & Float'Image(xStart + xSize));
            Ada.Text_IO.Put_Line(self.io, "," & Float'Image(yPos) & ");");
         else
            Ada.Text_IO.Put(self.io, "\draw (" & Float'Image(xStart) & ",");
            Ada.Text_IO.Put(self.io, Float'Image(yPos) & ") -- (" & Float'Image(xStart + tick_size));
            Ada.Text_IO.Put(self.io, "," & Float'Image(yPos) & ");");
            Ada.Text_IO.Put(self.io, "\draw (" & Float'Image(xStart + xSize - tick_size) & ",");
            Ada.Text_IO.Put(self.io, Float'Image(yPos) & ") -- (" & Float'Image(xStart + xSize));
            Ada.Text_IO.Put_Line(self.io, "," & Float'Image(yPos) & ");");
         end if;
         Ada.Text_IO.Put(self.io, "\node [rotate=90] at (" & Float'Image(xStart - 0.5) & ",");
         Ada.Text_IO.Put(self.io, Float'Image(yPos) & ") {");
         Ada.Float_Text_IO.Put(self.io, self.yMin + float(y)*(self.yMax - self.yMin)/float(yTicks), 2, 2, 0);
         Ada.Text_IO.Put_Line(self.io, "};");
      end loop;
   end;
   --
   --  Set Axis label
   --
   procedure label(self : in out linear_latex_plot_record; xLabel, yLabel : String) is
   xPos : constant Float := xStart - 1.0;
   yPos : constant Float := yStart + ySize/2.0;
   begin
      if not self.valid then
         return;
      end if;
      Ada.Text_IO.Put(self.io, "\node at (" & Float'Image(xStart + xSize/2.0) & ",");
      Ada.Text_IO.Put_Line(self.io, Float'Image(yStart - 1.0) & ") {" & xLabel & "};");
      Ada.Text_IO.Put(self.io, "\node [rotate=90] at (" & Float'Image(xPos) & ",");
      Ada.Text_IO.Put_Line(self.io, Float'Image(yPos) & ") {" & yLabel & "};");
   end;
   --
   --  Set title
   --
   procedure title(self : in out linear_latex_plot_record; title : String) is
   begin
      if not self.valid then
         return;
      end if;
      Ada.Text_IO.Put(self.io, "\node at (" & Float'Image(xStart + xSize/2.0) & ",");
      Ada.Text_IO.Put_Line(self.io, Float'Image(yStart + ySize + 0.5) & ") {" & title & "};");
   end;
   --
   --  Plot lines
   --
   procedure draw_line(self : in out linear_latex_plot_record;
                     points : BBS.Numerical.plot.point_list; color : String) is
      p : BBS.Numerical.plot.point;
   begin
      if not self.valid then
         return;
      end if;
      p := points(0);
      Ada.Text_IO.Put(self.io, "\draw [" & color & "] (" & Float'Image(self.xTranslate(p.x)) &
                     "," & Float'Image(self.yTranslate(p.y)));
      for x in points'First + 1 .. points'Last loop
         p := points(x);
         Ada.Text_IO.Put(self.io, ") -- (" & Float'Image(self.xTranslate(p.x)) &
                     "," & Float'Image(self.yTranslate(p.y)));
      end loop;
      Ada.Text_IO.Put_Line(self.io, ");");
   end;
   --
   --  Plot point(s)
   --
   procedure draw_point(self : in out linear_latex_plot_record;
                        p : BBS.Numerical.plot.point; size : Float;
                        color : String) is
   begin
      if not self.valid then
         return;
      end if;
      Ada.Text_IO.Put(self.io, "\path [fill=" & color & "] (" & Float'Image(self.xTranslate(p.x)));
      Ada.Text_IO.Put(self.io, "," & Float'Image(self.yTranslate(p.y)) & ") circle [radius=");
      Ada.Text_IO.Put_Line(self.io, Float'Image(size) & "];");
   end;
   --
   procedure draw_point(self : in out linear_latex_plot_record;
                        points : BBS.Numerical.plot.point_list; size : Float;
                        color : String) is
   begin
      if not self.valid then
         return;
      end if;
      for p of points loop
         Ada.Text_IO.Put(self.io, "\path [fill=" & color & "] (" & Float'Image(self.xTranslate(p.x)));
         Ada.Text_IO.Put(self.io, "," & Float'Image(self.yTranslate(p.y)) & ") circle [radius=");
         Ada.Text_IO.Put_Line(self.io, Float'Image(size) & "];");
      end loop;
   end;
   --
   --  Draw text at a point
   --
   procedure draw_text(self : in out linear_latex_plot_record;
                        p : BBS.Numerical.plot.point; color, text : String) is
   begin
      if not self.valid then
         return;
      end if;
      Ada.Text_IO.Put(self.io, "\node [right, " & color & "] at (" & Float'Image(self.xTranslate(p.x)) & ",");
      Ada.Text_IO.Put_Line(self.io, Float'Image((self.yTranslate(p.y))) &  ") {" & text & "};");
   end;
   --
   --  Glyph helper functions
   --
   procedure draw_glyph_plus(self : in out linear_latex_plot_record;
                              xPos, yPos, size : Float; color : String) is
   begin
      Ada.Text_IO.Put(self.io, "\draw[" & color & "] (" & Float'Image(xPos - size));
      Ada.Text_IO.Put(self.io, "," & Float'Image(yPos) & ") -- (");
      Ada.Text_IO.Put(self.io, Float'Image(xPos + size) & "," & Float'Image(yPos) & ");");
      Ada.Text_IO.Put_Line(self.io, "\draw[" & color & "] (" & Float'Image(xPos));
      Ada.Text_IO.Put(self.io, "," & Float'Image(yPos - size) & ") -- (");
      Ada.Text_IO.Put_Line(self.io, Float'Image(xPos) & "," & Float'Image(yPos + size) & ");");
   end;
   --
   procedure draw_glyph_X(self : in out linear_latex_plot_record;
                              xPos, yPos, size : Float; color : String) is
   begin
      Ada.Text_IO.Put(self.io, "\draw [" & color & "] (" & Float'Image(xPos - size));
      Ada.Text_IO.Put(self.io, "," & Float'Image(yPos - size) & ") -- (");
      Ada.Text_IO.Put_Line(self.io, Float'Image(xPos + size) & "," & Float'Image(yPos + size) & ");");
      Ada.Text_IO.Put(self.io, "\draw [" & color & "] (" & Float'Image(xPos + size));
      Ada.Text_IO.Put(self.io, "," & Float'Image(yPos - size) & ") -- (");
      Ada.Text_IO.Put_line(self.io, Float'Image(xPos - size) & "," & Float'Image(yPos + size) & ");");
   end;
   --
   procedure draw_glyph_asterisk(self : in out linear_latex_plot_record;
                              xPos, yPos, size : Float; color : String) is
   begin
      self.draw_glyph_plus(xPos, yPos, size, color);
      self.draw_glyph_X(xPos, yPos, size, color);
   end;
   --
   procedure draw_glyph_box(self : in out linear_latex_plot_record;
                           xPos, yPos, size : Float; color : String) is
   begin
      Ada.Text_IO.Put(self.io, "\draw [" & color & "] (" & Float'Image(xPos + size));
      Ada.Text_IO.Put(self.io, "," & Float'Image(yPos + size) & ") -- (");
      Ada.Text_IO.Put(self.io, Float'Image(xPos - size) & "," & Float'Image(yPos + size));
      Ada.Text_IO.Put(self.io, ") -- (" & Float'Image(xPos - size) & ",");
      Ada.Text_IO.Put(self.io, Float'Image(yPos - size) & ") -- (" & Float'Image(xPos + size));
      Ada.Text_IO.Put(self.io, "," & Float'Image(yPos - size) & ") -- (");
      Ada.Text_IO.Put_Line(self.io, Float'Image(xPos + size) & "," & Float'Image(yPos + size) & ");");
   end;
   --
   procedure draw_glyph_diamond(self : in out linear_latex_plot_record;
                              xPos, yPos, size : Float; color : String) is
   begin
      Ada.Text_IO.Put(self.io, "\draw [" & color & "] (" & Float'Image(xPos));
      Ada.Text_IO.Put(self.io, "," & Float'Image(yPos + size) & ") -- (");
      Ada.Text_IO.Put(self.io, Float'Image(xPos - size) & "," & Float'Image(yPos));
      Ada.Text_IO.Put(self.io, ") -- (" & Float'Image(xPos) & ",");
      Ada.Text_IO.Put(self.io, Float'Image(yPos - size) & ") -- (" & Float'Image(xPos + size));
      Ada.Text_IO.Put(self.io, "," & Float'Image(yPos) & ") -- (");
      Ada.Text_IO.Put_Line(self.io, Float'Image(xPos) & "," & Float'Image(yPos + size) & ");");
   end;
   --
   procedure draw_glyph_octagon(self : in out linear_latex_plot_record;
                              xPos, yPos, size : Float; color : String) is
   begin
      Ada.Text_IO.Put(self.io, "\draw [" & color & "] (" & Float'Image(xPos - size/2.0));
      Ada.Text_IO.Put(self.io, "," & Float'Image(yPos + size) & ") -- (");
      Ada.Text_IO.Put(self.io, Float'Image(xPos + size/2.0) & "," & Float'Image(yPos + size));
      Ada.Text_IO.Put(self.io, ") -- (" & Float'Image(xPos + size) & ",");
      Ada.Text_IO.Put(self.io, Float'Image(yPos + size/2.0) & ") -- (" & Float'Image(xPos + size));
      Ada.Text_IO.Put(self.io, "," & Float'Image(yPos - size/2.0) & ") -- (");
      Ada.Text_IO.Put(self.io, Float'Image(xPos + size/2.0) & "," & Float'Image(yPos - size));
      Ada.Text_IO.Put(self.io, ") -- (" & Float'Image(xPos - size/2.0) & ",");
      Ada.Text_IO.Put(self.io, Float'Image(yPos - size) & ") -- (" & Float'Image(xPos - size));
      Ada.Text_IO.Put(self.io, "," & Float'Image(yPos - size/2.0) & ") -- (");
      Ada.Text_IO.Put(self.io, Float'Image(xPos - size) & "," & Float'Image(yPos + size/2.0));
      Ada.Text_IO.Put(self.io, ") -- (" & Float'Image(xPos - size/2.0) & ",");
      Ada.Text_IO.Put(self.io, Float'Image(yPos + size) & ");");
   end;
   --
   procedure select_glyph(self : in out linear_latex_plot_record;
                        g : BBS.Numerical.plot.glyph;
                        xPos, yPos, size : Float; color : String) is
   begin
      case g is
         when BBS.Numerical.plot.glyph_plus =>
            self.draw_glyph_plus(xPos, yPos, size, color);
         when BBS.Numerical.plot.glyph_X =>
            self.draw_glyph_X(xPos, yPos, size, color);
         when BBS.Numerical.plot.glyph_asterisk =>
            self.draw_glyph_asterisk(xPos, yPos, size, color);
         when BBS.Numerical.plot.glyph_box =>
            self.draw_glyph_box(xPos, yPos, size, color);
         when BBS.Numerical.plot.glyph_diamond =>
            self.draw_glyph_diamond(xPos, yPos, size, color);
         when BBS.Numerical.plot.glyph_octagon =>
            self.draw_glyph_octagon(xPos, yPos, size, color);
      end case;
   end;
   --
   procedure draw_err(self : in out linear_latex_plot_record;
                       xPos, yPos, size, err : Float; color : String) is
   begin
      Ada.Text_IO.Put(self.io, "\draw [" & color & "] (" & Float'Image(xPos - size));
      Ada.Text_IO.Put(self.io, "," & Float'Image(yPos - err) & ") -- (");
      Ada.Text_IO.Put(self.io, Float'Image(xPos - size) & "," & Float'Image(yPos - err) & ");");
      Ada.Text_IO.Put(self.io, "\draw [" & color & "] (" & Float'Image(xPos + size));
      Ada.Text_IO.Put(self.io, "," & Float'Image(yPos + err) & ") -- (");
      Ada.Text_IO.Put(self.io, Float'Image(xPos + size) & "," & Float'Image(yPos + err) & ");");
      Ada.Text_IO.Put(self.io, "\draw [" & color & "] (" & Float'Image(xPos - size));
      Ada.Text_IO.Put(self.io, "," & Float'Image(yPos ) & ") -- (");
      Ada.Text_IO.Put(self.io, Float'Image(xPos + size) & "," & Float'Image(yPos) & ");");
   end;
   --
   --  Draw a glyph at a point
   --
   procedure draw_glyph(self : in out linear_latex_plot_record;
                        p : BBS.Numerical.plot.point;
                        g : BBS.Numerical.plot.glyph; color : String) is
      size : constant Float := glyph_size;
   begin
      if not self.valid then
         return;
      end if;
      self.select_glyph(g, self.xTranslate(p.x), self.yTranslate(p.y), size, color);
   end;
   --
   procedure draw_glyph(self : in out linear_latex_plot_record;
                        points : BBS.Numerical.plot.point_list;
                        g : BBS.Numerical.plot.glyph; color : String) is
      size : constant Float := glyph_size;
   begin
      if not self.valid then
         return;
      end if;
      for p of points loop
         self.select_glyph(g, self.xTranslate(p.x), self.yTranslate(p.y), size, color);
      end loop;
   end;
   --
   --  Draw glyphs with error bars
   --
   procedure draw_glyph(self : in out linear_latex_plot_record;
                        p : BBS.Numerical.plot.point_err;
                        g : BBS.Numerical.plot.glyph; color : String) is
      xPos : constant Float := self.xTranslate(p.x);
      yPos : constant Float := self.yTranslate(p.y);
      err  : constant Float := p.e*ySize/(self.yMax - self.yMin);
      size : constant Float := glyph_size;
   begin
      if not self.valid then
         return;
      end if;
      self.select_glyph(g, xPos, yPos, size, color);
      self.draw_err(xPos, yPos, size, err, color);
   end;
   --
   procedure draw_glyph(self : in out linear_latex_plot_record;
                        points : BBS.Numerical.plot.point_err_list;
                        g : BBS.Numerical.plot.glyph; color : String) is
      xPos : Float;
      yPos : Float;
      err  : Float;
      size : constant Float := glyph_size;
   begin
      if not self.valid then
         return;
      end if;
      for p of points loop
         xPos := self.xTranslate(p.x);
         yPos := self.yTranslate(p.y);
         err  := p.e*ySize/(self.yMax - self.yMin);
         self.select_glyph(g, xPos, yPos, size, color);
         self.draw_err(xPos, yPos, size, err, color);
      end loop;
   end;
   --
   --  Close the plot
   --
   procedure end_plot(self : in out linear_latex_plot_record) is
   begin
      if self.valid then
         Ada.Text_IO.Put_Line(self.io, "\end{tikzpicture}");
         Ada.Text_IO.Close(self.io);
      end if;
      self.valid := False;
   end;
end BBS.Numerical.plot_latex_linear;
