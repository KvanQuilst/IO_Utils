-------------------------------------------------------------------------------
--                                                                           --
--                                IO_Utils                                   --
--                                                                           --
--                         IO_Utils . Ansi (Spec)                            --
--                                                                           --
--                      Copyright (C) 2024 Dylan Eskew                       --
--                                                                           --
-- This file contains terminal procedures for setting style, manipulating    --
-- the terminal cursor, and erasing elements of the terminal.                --
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
package IO_Utils.Ansi is

   type Color is (Black, Red, Green, Yellow,
                  Blue, Magenta, Cyan, White, Default,
                  B_Black, B_Red, B_Green, B_Yellow,
                  B_Blue, B_Magenta, B_Cyan, B_White);
   for Color use (
      Black     => 30,
      Red       => 31,
      Green     => 32,
      Yellow    => 33,
      Blue      => 34,
      Magenta   => 35,
      Cyan      => 36,
      White     => 37,
      -- Reserved for 8bit and Truecolor --
      Default   => 39,
      B_Black   => 90,
      B_Red     => 91,
      B_Green   => 92,
      B_Yellow  => 93,
      B_Blue    => 94,
      B_Magenta => 95,
      B_Cyan    => 96,
      B_White   => 97);

   subtype Color_8 is Integer range 0 .. 255;

   type Color_RGB is record
      Red   : Color_8;
      Green : Color_8;
      Blue  : Color_8;
   end record;

   type Style is (Bold, Dim, Italic, Underline,
                  Blinking, Inverse, Hidden, Strikethrough);
   for Style use (
      Bold => 1,
      Dim  => 2,
      Italic => 3,
      Underline => 4,
      Blinking  => 5,
      Inverse   => 7,
      Hidden    => 8,
      Strikethrough => 9);

   type Style_Set is array (Style'Range) of Boolean
      with Default_Component_Value => False;

   ------------------
   -- Escape Codes --
   ------------------

   ESC : constant Character := Character'Val (27);
   CSI : constant String    := ESC & "[";

   --------------
   -- Coloring --
   --------------

   procedure Set_Fg    (Fg : Color)   with Inline;
   procedure Set_Fg    (Fg : Color_8) with Inline;
   procedure Set_Fg    (Fg : Color_RGB);

   procedure Set_Bg    (Bg : Color_8) with Inline;
   procedure Set_Bg    (Bg : Color)   with Inline;
   procedure Set_Bg    (Bg : Color_RGB);

   procedure Set_Color (Fg : Color_8;
                        Bg : Color_8) with Inline;
   procedure Set_Color (Fg : Color;
                        Bg : Color)   with Inline;
   procedure Set_Color (Fg : Color_RGB;
                        Bg : Color_RGB);

   -------------
   -- Styling --
   -------------

   procedure Set_Style (S        : Style) with Inline;
   procedure Set_Style (Styles   : Style_Set;
                        Override : Boolean := False);

   procedure Unset_Style (S        : Style);
   procedure Unset_Style (Styles   : Style_Set);

   procedure Reset_All with Inline;

   -------------
   -- Erasing --
   -------------

   procedure Erase_All                 with Inline;
   procedure Erase_Screen              with Inline;
   procedure Erase_Screen_To_Beginning with Inline;
   procedure Erase_Screen_To_End       with Inline;

   procedure Erase_Line              with Inline;
   procedure Erase_Line_To_Beginning with Inline;
   procedure Erase_Line_To_End       with Inline;

   --------------------
   -- Cursor Control --
   --------------------

   procedure Cursor_Home with Inline;
   procedure Cursor_Set        (Line : Positive;
                                Col  : Natural) with Inline;
   procedure Cursor_Col_Set    (Col  : Natural) with Inline;

   procedure Cursor_Line_Move   (Num_Lines : Integer) with Inline;
   procedure Cursor_Col_Move    (Num_Cols  : Integer) with Inline;

   procedure Cursor_Save_Position    with Inline;
   procedure Cursor_Restore_Position with Inline;

private

   procedure Set_RGB (C : Color_RGB) with Inline;

end IO_Utils.Ansi;
