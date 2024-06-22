-------------------------------------------------------------------------------
--                                                                           --
--                                IO_Utils                                   --
--                                                                           --
--                        IO_Utils . User_IO (Body)                          --
--                                                                           --
--                      Copyright (C) 2024 Dylan Eskew                       --
--                                                                           --
-- This file contains functions for obtaining checked user input in a pretty --
-- format.                                                                   --
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
with Ada.Text_IO; use Ada.Text_IO;

package body IO_Utils.User_IO is

   ------------------------
   -- Checked User Input --
   ------------------------

   function To_Opt (Length : Positive) return Character is
      (Character'Val (96 + Length));

   function Get_Option (Options : Str_Arr) return Character is
      Colors : constant Color_Arr (Options'Range) :=
         (others => (Color_T, Default));
   begin
      return Get_Option (Options, Colors);
   end Get_Option;

   function Get_Option (Options : Str_Arr;
                        Colors  : Color_Arr) return Character is
      C   : Character;
      Opt : Character := '0';
   begin
      Reset_All;
      Separator;
      Put_Line (Default_Option_Prompt);
      New_Line;
      loop
         -- List Options --
         for I in Options'Range loop
            if Character'Pos (Opt) - 96 = I
            then
               Put ("[" & To_Opt (I) & "] ");
               Set_Fg (Colors (I));
            else
               Put (" " & To_Opt (I) & ". ");
               Set_Fg (Default_Unselected_Color);
            end if;
            Put (To_String (Options (I)));
            Erase_Line_To_End;
            Reset_All;
            New_Line;
         end loop;
         Separator;

         -- Get Input --
         Get_Immediate (C);
         exit when Opt >= 'a' and then
                   Opt <= To_Opt (Options'Length) and then
                   Character'Pos (C) = 10;

         if C >= 'a' and then C <= To_Opt (Options'Length)
         then
            Opt := C;
         end if;

         -- Reposition to Top --
         Cursor_Line_Move (0 - (Options'Length + 1));
      end loop;

      return Opt;
   end Get_Option;

   function Get_Options (Options : Str_Arr) return Char_Arr is
      Colors : constant Color_Arr (Options'Range) :=
         (others => (Color_T, Default));
   begin
      return Get_Options (Options, Colors);
   end Get_Options;

   function Get_Options (Options : Str_Arr;
                         Colors  : Color_Arr) return Char_Arr is
      C       : Character;
      Opt_Set : Bool_Arr (Options'Range) := (others => False);
      Opts    : Char_Arr (Options'Range);
      Total   : Natural := 0;
   begin
      Separator;
      Put_Line (Default_Option_Prompt);
      New_Line;
      loop
         -- List Options --
         for I in Options'Range loop
            if Opt_Set (I)
            then
               Put ("[" & To_Opt (I) & "] ");
               Set_Fg (Colors (I));
            else
               Put (" " & To_Opt (I) & ". ");
            end if;
            Put (To_String (Options (I)));
            Erase_Line_To_End;
            Reset_All;
            New_Line;
         end loop;
         Separator;

         -- Get Input --
         Get_Immediate (C);
         exit when (for some I in Opt_Set'Range => Opt_Set (I)) and then
                   Character'Pos (C) = 10;

         if C >= 'a' and then C <= To_Opt (Options'Length)
         then
            Opt_Set (Character'Pos (C) - 96) :=
               not Opt_Set (Character'Pos (C) - 96);
         end if;

         -- Reposition to Top --
         Cursor_Line_Move (0 - (Options'Length + 1));
      end loop;

      for Opt in Opt_Set'Range loop
         if Opt_Set (Opt)
         then
            Total := Total + 1;
            Opts (Total) := To_Opt (Opt - 1);
         end if;
      end loop;

      return Opts (1 .. Total);
   end Get_Options;

   -----------------------
   -- Inteface Elements --
   -----------------------

   procedure Continue is
      Line : Unbounded_String;
      pragma Unreferenced (Line);
   begin
      New_Line;
      Put ("Press enter to continue...");
      Line := To_Unbounded_String (Get_Line);
      New_Line;
   end Continue;

   procedure Separator is
   begin
      Put_Line (To_String (Default_Width * Default_Separator));
   end Separator;

end IO_Utils.User_IO;
