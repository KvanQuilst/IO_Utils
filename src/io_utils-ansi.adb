-------------------------------------------------------------------------------
--                                                                           --
--                                IO_Utils                                   --
--                                                                           --
--                         IO_Utils . Ansi (Body)                            --
--                                                                           --
--                      Copyright (C) 2024 Dylan Eskew                       --
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
with Ada.Integer_Text_IO;
with Ada.Text_IO; use Ada.Text_IO;

package body IO_Utils.Ansi is
   -- Set default Integer width to 0 --
   procedure Put (Item  : Integer;
                  Width : Field := 0;
                  Base  : Number_Base := Ada.Integer_Text_IO.Default_Base)
      renames Ada.Integer_Text_IO.Put;

   --------------
   -- Coloring --
   --------------

   procedure Set_Fg (Fg : Color) is
   begin
      Put (CSI);
      Put (Color'Enum_Rep (Fg));
      Put ("m");
   end Set_Fg;

   procedure Set_Fg (Fg : Color_8) is
   begin
      Put (CSI & "38;5;");
      Put (Fg);
      Put ("m");
   end Set_Fg;

   procedure Set_Fg (Fg : Color_RGB) is
   begin
      Put (CSI & "38;2;");
      Set_RGB (Fg);
      Put ("m");
   end Set_Fg;

   procedure Set_Fg (Fg : Color_Elem) is
   begin
      case Fg.T is
         when Color_T     => Set_Fg (Fg.C);
         when Color_8_T   => Set_Fg (Fg.C8);
         when Color_RGB_T => Set_Fg (Fg.CRGB);
      end case;
   end Set_Fg;

   procedure Set_Bg (Bg : Color_8) is
   begin
      Put (CSI & "48;5;");
      Put (Bg);
      Put ("m");
   end Set_Bg;

   procedure Set_Bg (Bg : Color) is
   begin
      Put (CSI);
      Put (Color'Enum_Rep (Bg) + 10);
      Put ("m");
   end Set_Bg;

   procedure Set_Bg (Bg : Color_RGB) is
   begin
      Put (CSI & "48;2;");
      Set_RGB (Bg);
      Put ("m");
   end Set_Bg;

   procedure Set_Bg (Bg : Color_Elem) is
   begin
      case Bg.T is
         when Color_T     => Set_Bg (Bg.C);
         when Color_8_T   => Set_Bg (Bg.C8);
         when Color_RGB_T => Set_Bg (Bg.CRGB);
      end case;
   end Set_Bg;

   procedure Set_Color (Fg : Color;
                        Bg : Color) is
   begin
      Put (CSI);
      Put (Color'Enum_Rep (Fg));
      Put (";");
      Put (Color'Enum_Rep (Bg) + 10);
      Put ("m");
   end Set_Color;

   procedure Set_Color (Fg : Color_8;
                        Bg : Color_8) is
   begin
      Put (CSI & "38;5;");
      Put (Fg);
      Put ("48;5;");
      Put (Bg);
      Put ("m");
   end Set_Color;

   procedure Set_Color (Fg : Color_RGB;
                        Bg : Color_RGB) is
   begin
      Put (CSI & "38;2;");
      Set_RGB (Fg);
      Put (";48;2;");
      Set_RGB (Bg);
      Put ("m");
   end Set_Color;

   procedure Set_Color (Fg : Color_Elem;
                        Bg : Color_Elem) is
   begin
      Set_Fg (Fg);
      Set_Bg (Bg);
   end Set_Color;

   -- Assumes '^[' set before call and 'm' after call --
   procedure Set_RGB (C : Color_RGB) is
   begin
      Put (C.Red);
      Put (";");
      Put (C.Green);
      Put (";");
      Put (C.Blue);
   end Set_RGB;

   -------------
   -- Styling --
   -------------

   procedure Set_Style (S : Style) is
   begin
      Put (CSI);
      Put (Style'Enum_Rep (S));
      Put ("m");
   end Set_Style;

   procedure Set_Style (Styles   : Style_Set;
                         Override : Boolean := False) is
      Semi_Colon : Boolean := False;
   begin
      if not Override and then
         (for all S in Styles'Range => Styles (S) = False)
      then
         return;
      end if;

      Put (CSI);
      for S in Styles'Range loop
         if Semi_Colon then
            Put (";");
         end if;

         Semi_Colon := False;

         if Styles (S) then
            Put (Style'Enum_Rep (S));
            Semi_Colon := True;

         elsif Override and then not Styles (S) then
            if S = Bold then
               Put (22);
            else
               Put (Style'Enum_Rep (S) + 20);
            end if;
            Semi_Colon := True;
         end if;
      end loop;
      Put ("m");
   end Set_Style;

   procedure Unset_Style (S : Style) is
   begin
      Put (CSI);
      if S = Bold then
         Put (22);
      else
         Put (Style'Enum_Rep (S) + 20);
      end if;
      Put ("m");
   end Unset_Style;

   procedure Unset_Style (Styles : Style_Set) is
      Semi_Colon : Boolean := False;
   begin
      if (for all S in Styles'Range => Styles (S) = False) then
         return;
      end if;

      Put (CSI);
      for S in Styles'Range loop
         if Semi_Colon then
            Put (";");
         end if;

         Semi_Colon := False;

         if Styles (S) then
            Put (Style'Enum_Rep (S) + 20);
            Semi_Colon := True;
         end if;
      end loop;
      Put ("m");
   end Unset_Style;

   procedure Reset_All is
   begin
      Put (CSI & "0m");
   end Reset_All;

   -------------
   -- Erasing --
   -------------

   procedure Erase_All is
   begin
      Put (CSI & "3J");
   end Erase_All;

   procedure Erase_Screen is
   begin
      Put (CSI & "2J");
   end Erase_Screen;

   procedure Erase_Screen_To_Beginning is
   begin
      Put (CSI & "1J");
   end Erase_Screen_To_Beginning;

   procedure Erase_Screen_To_End is
   begin
      Put (CSI & "0J");
   end Erase_Screen_To_End;

   procedure Erase_Line is
   begin
      Put (CSI & "2K");
   end Erase_Line;

   procedure Erase_Line_To_Beginning is
   begin
      Put (CSI & "1K");
   end Erase_Line_To_Beginning;

   procedure Erase_Line_To_End is
   begin
      Put (CSI & "0K");
   end Erase_Line_To_End;

   --------------------
   -- Cursor Control --
   --------------------

   procedure Cursor_Home is
   begin
      Put (CSI & "H");
   end Cursor_Home;

   procedure Cursor_Set (Line : Positive;
                         Col  : Natural) is
   begin
      Put (CSI);
      Put (Line);
      Put (";");
      Put (Col);
      Put ("H");
   end Cursor_Set;

   procedure Cursor_Col_Set (Col : Natural) is
   begin
      Put (CSI);
      Put (Col);
      Put ("G");
   end Cursor_Col_Set;

   procedure Cursor_Line_Move (Num_Lines : Integer) is
   begin
      if Num_Lines = 0 then
         return;
      end if;

      Put (CSI);
      Put ((if Num_Lines < 0
            then 0 - Num_Lines
            else Num_Lines));
      Put ((if Num_Lines < 0
            then "A"
            else "B"));
   end Cursor_Line_Move;

   procedure Cursor_Col_Move (Num_Cols : Integer) is
   begin
      if Num_Cols = 0 then
         return;
      end if;

      Put (CSI);
      Put ((if Num_Cols < 0
            then 0 - Num_Cols
            else Num_Cols));
      Put ((if Num_Cols < 0
            then "D"
            else "C"));
   end Cursor_Col_Move;

   procedure Cursor_Save_Position is
   begin
      Put (ESC & "7");
   end Cursor_Save_Position;

   procedure Cursor_Restore_Position is
   begin
      Put (ESC & "8");
   end Cursor_Restore_Position;

end IO_Utils.Ansi;
