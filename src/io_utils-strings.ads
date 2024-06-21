-------------------------------------------------------------------------------
--                                                                           --
--                                IO_Utils                                   --
--                                                                           --
--                        IO_Utils . Strings (Spec)                          --
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
with IO_Utils.Ansi; use IO_Utils.Ansi;

package IO_Utils.Strings is

   --------------
   -- Coloring --
   --------------

   function Set_Fg    (Str : String;
                       Fg  : Color)     return String;
   function Set_Fg    (Str : String;
                       Fg  : Color_8)   return String;
   function Set_Fg    (Str : String;
                       Fg  : Color_RGB) return String;

   function Set_Bg    (Str : String;
                       Bg  : Color_8)   return String;
   function Set_Bg    (Str : String;
                       Bg  : Color)     return String;
   function Set_Bg    (Str : String;
                       Bg  : Color_RGB) return String;

   function Set_Color (Str : String;
                       Fg  : Color_8;
                       Bg  : Color_8)   return String;
   function Set_Color (Str : String;
                       Fg  : Color;
                       Bg  : Color)     return String;
   function Set_Color (Str : String;
                       Fg  : Color_RGB;
                       Bg  : Color_RGB) return String;

end IO_Utils.Strings;
