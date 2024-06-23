-------------------------------------------------------------------------------
--                                                                           --
--                                IO_Utils                                   --
--                                                                           --
--                        IO_Utils . User_IO (Spec)                          --
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
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with IO_Utils.Ansi; use IO_Utils.Ansi;

package IO_Utils.User_IO is

   type Bool_Arr  is array (Positive range <>) of Boolean;
   type Char_Arr  is array (Positive range <>) of Character;
   type Color_Arr is array (Positive range <>) of Color_Elem;
   type Int_Arr   is array (Positive range <>) of Integer;
   type Str_Arr   is array (Positive range <>) of Unbounded_String;

   ------------------------
   -- Checked User Input --
   --                    --
   -- Ask before calling --
   ------------------------

   Default_Option_Prompt    : String      := "Select from these options:";
   Default_Unselected_Color : Color_Elem  := (Color_T, Default);

   function Get_Option (Options : Str_Arr) return Character
      with Pre => Options'Length > 0;
   function Get_Option (Options : Str_Arr;
                        Colors  : Color_Arr)  return Character
      with Pre => Options'Length > 0 and then Options'Length = Colors'Length;

   function Get_Options (Options : Str_Arr) return Char_Arr
      with Pre => Options'Length > 0;
   function Get_Options (Options : Str_Arr;
                         Colors  : Color_Arr)  return Char_Arr
      with Pre => Options'Length > 0 and then Options'Length = Colors'Length;

   function Get_Integer (Low, High : Integer) return Integer;

   ------------------------
   -- Interface Elements --
   ------------------------

   Default_Width : Positive := 40;
   Default_Separator    : Character := '-';

   procedure Continue;
   procedure Separator with Inline;

end IO_Utils.User_IO;
