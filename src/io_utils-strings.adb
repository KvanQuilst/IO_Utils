-------------------------------------------------------------------------------
--                                                                           --
--                                IO_Utils                                   --
--                                                                           --
--                        IO_Utils . Strings (Body)                          --
--                                                                           --
--                      Copyright (C) 2024 Dylan Eskew                       --
--                                                                           --
-- This file contains functions for adding ANSI escape sequences to strings. --
--                                                                           --
-- IO_Utils is free software: you can redistribute it and/or modify it       --
-- under the terms of the GNU General Public License as published by the     --
-- Free Software Foundation, either version 3 of the License, or (at your    --
-- option) any later version.                                                --
--                                                                           --
-- IO_Utils is distributed in the hope that it will be useful, but WITHOUT   --
-- ANY WARRANTY; wihtout even the implied warranty of MERCHANTABILITY or     --
-- FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for  --
-- more details.                                                             --
--                                                                           --
-- You should have received a copy of the GNU General Public License along   --
-- with IO_Utils. If not, see <https://www.gnu.org/licenses/>.               --
-------------------------------------------------------------------------------
package body IO_Utils.Strings is

   --------------
   -- Coloring --
   --------------
   function Set (Str, Val_Str : String) return String is
      (CSI & Val_Str & "m" & Str & CSI & "0m");

   function Set_Fg (Str : String;
                    Fg  : Color) return String is
      Val : constant String := Color'Enum_Rep (Fg)'Image (2 .. 3);
   begin
      return Set (Str, Val);
   end Set_Fg;

   function Set_Fg (Str : String;
                    Fg  : Color_8) return String is
      Size : constant Positive := (case Fg is
                                    when 0 .. 9     => 1,
                                    when 10 .. 99   => 2,
                                    when 100 .. 255 => 3);
      Val  : constant String   := Fg'Image (2 .. Size + 1);
   begin
      return Set (Str, "38;5;" & Val);
   end Set_Fg;

   function Set_Fg (Str : String;
                    Fg  : Color_RGB) return String is
      Size_R : constant Positive := (case Fg.Red is
                                       when 0 .. 9     => 1,
                                       when 10 .. 99   => 2,
                                       when 100 .. 255 => 3);
      Size_G : constant Positive := (case Fg.Green is
                                       when 0 .. 9     => 1,
                                       when 10 .. 99   => 2,
                                       when 100 .. 255 => 3);
      Size_B : constant Positive := (case Fg.Blue is
                                       when 0 .. 9     => 1,
                                       when 10 .. 99   => 2,
                                       when 100 .. 255 => 3);
      Val_R  : constant String   := Fg.Red'Image   (2 .. Size_R + 1);
      Val_G  : constant String   := Fg.Green'Image (2 .. Size_G + 1);
      Val_B  : constant String   := Fg.Blue'Image  (2 .. Size_B + 1);
   begin
      return Set (Str, "38;2;" & Val_R & ";" & Val_G & ";" & Val_B);
   end Set_Fg;

   function Set_Fg (Str : String;
                    Fg  : Color_Elem) return String is
      (case Fg.T is
         when Color_T     => Set_Fg (Str, Fg.C),
         when Color_8_T   => Set_Fg (Str, Fg.C8),
         when Color_RGB_T => Set_Fg (Str, Fg.CRGB));

   function Set_Bg (Str : String;
                    Bg  : Color) return String is
      Bg_Val : constant Integer := Color'Enum_Rep (Bg) + 10;
      Val    : constant String  := Bg_Val'Image (2 .. 3);
   begin
      return Set (Str, Val);
   end Set_Bg;

   function Set_Bg (Str : String;
                    Bg  : Color_8) return String is
      Size : constant Positive := (case Bg is
                                    when 0 .. 9     => 1,
                                    when 10 .. 99   => 2,
                                    when 100 .. 255 => 3);
      Val  : constant String   := Bg'Image (2 .. Size + 1);
   begin
      return Set (Str, "48;5;" & Val);
   end Set_Bg;

   function Set_Bg (Str : String;
                    Bg  : Color_Elem) return String is
      (case Bg.T is
         when Color_T     => Set_Bg (Str, Bg.C),
         when Color_8_T   => Set_Bg (Str, Bg.C8),
         when Color_RGB_T => Set_Bg (Str, Bg.CRGB));

   function Set_Bg (Str : String;
                    Bg  : Color_RGB) return String is
      Size_R : constant Positive := (case Bg.Red is
                                       when 0 .. 9     => 1,
                                       when 10 .. 99   => 2,
                                       when 100 .. 255 => 3);
      Size_G : constant Positive := (case Bg.Green is
                                       when 0 .. 9     => 1,
                                       when 10 .. 99   => 2,
                                       when 100 .. 255 => 3);
      Size_B : constant Positive := (case Bg.Blue is
                                       when 0 .. 9     => 1,
                                       when 10 .. 99   => 2,
                                       when 100 .. 255 => 3);
      Val_R  : constant String   := Bg.Red'Image   (2 .. Size_R + 1);
      Val_G  : constant String   := Bg.Green'Image (2 .. Size_G + 1);
      Val_B  : constant String   := Bg.Blue'Image  (2 .. Size_B + 1);
   begin
      return Set (Str, "48;2;" & Val_R & ";" & Val_G & ";" & Val_B);
   end Set_Bg;

   function Set_Color (Str : String;
                       Fg  : Color;
                       Bg  : Color) return String is
      Bg_Val : constant Integer := Color'Enum_Rep (Bg) + 10;
      Val_Fg : constant String  := Color'Enum_Rep (Fg)'Image (2 .. 3);
      Val_Bg : constant String  := Bg_Val'Image (2 .. 3);
   begin
      return Set (Str, Val_Fg & ";" & Val_Bg);
   end Set_Color;

   function Set_Color (Str : String;
                       Fg  : Color_8;
                       Bg  : Color_8) return String is
      Size_Fg : constant Positive := (case Fg is
                                       when 0 .. 9     => 1,
                                       when 10 .. 99   => 2,
                                       when 100 .. 255 => 3);
      Size_Bg : constant Positive := (case Bg is
                                       when 0 .. 9     => 1,
                                       when 10 .. 99   => 2,
                                       when 100 .. 255 => 3);
      Val_Fg : constant String    := Fg'Image (2 .. Size_Fg + 1);
      Val_Bg : constant String    := Bg'Image (2 .. Size_Bg + 1);
   begin
      return Set (Str, "38;5;" & Val_Fg & ";48;5;" & Val_Bg);
   end Set_Color;

   function Set_Color (Str : String;
                       Fg  : Color_RGB;
                       Bg  : Color_RGB) return String is
      Size_Fg_R : constant Positive := (case Fg.Red is
                                          when 0 .. 9     => 1,
                                          when 10 .. 99   => 2,
                                          when 100 .. 255 => 3);
      Size_Fg_G : constant Positive := (case Fg.Green is
                                          when 0 .. 9     => 1,
                                          when 10 .. 99   => 2,
                                          when 100 .. 255 => 3);
      Size_Fg_B : constant Positive := (case Fg.Blue is
                                          when 0 .. 9     => 1,
                                          when 10 .. 99   => 2,
                                          when 100 .. 255 => 3);
      Size_Bg_R : constant Positive := (case Bg.Red is
                                          when 0 .. 9     => 1,
                                          when 10 .. 99   => 2,
                                          when 100 .. 255 => 3);
      Size_Bg_G : constant Positive := (case Bg.Green is
                                          when 0 .. 9     => 1,
                                          when 10 .. 99   => 2,
                                          when 100 .. 255 => 3);
      Size_Bg_B : constant Positive := (case Bg.Blue is
                                          when 0 .. 9     => 1,
                                          when 10 .. 99   => 2,
                                          when 100 .. 255 => 3);
      Val_Fg_R  : constant String   := Fg.Red'Image   (2 .. Size_Fg_R + 1);
      Val_Fg_G  : constant String   := Fg.Green'Image (2 .. Size_Fg_G + 1);
      Val_Fg_B  : constant String   := Fg.Blue'Image  (2 .. Size_Fg_B + 1);
      Val_Bg_R  : constant String   := Bg.Red'Image   (2 .. Size_Bg_R + 1);
      Val_Bg_G  : constant String   := Bg.Green'Image (2 .. Size_Bg_G + 1);
      Val_Bg_B  : constant String   := Bg.Blue'Image  (2 .. Size_Bg_B + 1);
   begin
      return Set (Str,
                  "38;2;" & Val_Fg_R & ";" & Val_Fg_G & ";" & Val_Fg_B & ";"
                & "48;2;" & Val_Bg_R & ";" & Val_Bg_G & ";" & Val_Bg_B);
   end Set_Color;

   function Set_Color (Str : String;
                       Fg  : Color_Elem;
                       Bg  : Color_Elem) return String is
      (Set_Fg (Set_Bg (Str, Bg), Fg));

end IO_Utils.Strings;
