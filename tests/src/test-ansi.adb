with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO;

with IO_Utils.Ansi; use IO_Utils.Ansi;

package body Test.Ansi is

   procedure Put (Item  : Integer;
                  Width : Field := 0;
                  Base  : Number_Base := Ada.Integer_Text_IO.Default_Base)
      renames Ada.Integer_Text_IO.Put;

   ---------------------
   -- Coloring Testss --
   ---------------------

   procedure Coloring_Tests is
   begin
      Put_Line ("--------------------");
      Put_Line ("-- Coloring Tests --");
      Put_Line ("--------------------");
      Put_Line ("These output of these tests need to be verified visually.");
      Put_Line ("These tests also inherently test `Reset_All`");

      Set_Fg_Test;
      Set_Fg_8_Test;
      Set_Fg_RGB_Test;

      Set_Bg_Test;
      Set_Bg_8_Test;
      Set_Bg_RGB_Test;

      Set_Color_Test;
      Set_Color_8_Test;
      Set_Color_RGB_Test;
   end Coloring_Tests;

   procedure Set_Fg_Test is
   begin
      New_Line;
      Put_Line ("-- Set_Fg_Test --");
      New_Line;

      Put_Line (" Control:");
      New_Line;
      Put_Line ("  " & CSI & "30m██"
                     & CSI & "31m██"
                     & CSI & "32m██"
                     & CSI & "33m██"
                     & CSI & "34m██"
                     & CSI & "35m██"
                     & CSI & "36m██"
                     & CSI & "37m██"
                     & CSI & "39m██"
                     & CSI & "90m██"
                     & CSI & "91m██"
                     & CSI & "92m██"
                     & CSI & "93m██"
                     & CSI & "94m██"
                     & CSI & "95m██"
                     & CSI & "96m██"
                     & CSI & "97m██"
                     & CSI & "0m");

      Put_Line (" Set_Fg:");
      Put ("  ");
      for C in Color'Range loop
         Set_Fg (C);
         Put ("██");
      end loop;
      Reset_All;
      New_Line;

      New_Line;
   end Set_Fg_Test;

   procedure Set_Fg_8_Test is
   begin
      New_Line;
      Put_Line ("-- Set_Fg_8_Test --");
      New_Line;

      for I in 0 .. 8 loop
         Put_Line (" Control:");
         Put ("  ");
         for J in 0 .. 31 loop
            Put (CSI & "38;5;");
            Put (I * 8 + J);
            Put ("m██");
         end loop;
         Reset_All;
         New_Line;

         Put_Line (" Set_Fg:");
         Put ("  ");
         for J in 0 .. 31 loop
            Set_Fg (I * 8 + J);
            Put ("██");
         end loop;
         Reset_All;
         New_Line;

         New_Line;
      end loop;
   end Set_Fg_8_Test;

   procedure Set_Fg_RGB_Test is
      C : Color_RGB := (245, 145, 45);
   begin
      New_Line;
      Put_Line ("-- Set_Fg_RGB_Test --");
      New_Line;

      Put_Line (" Control:");
      Put_Line ("  " & CSI & "38;2;245;145;45m██");
      Reset_All;

      Put_Line (" Set_Fg:");
      Set_Fg (C);
      Put_Line ("  ██");
      Reset_All;

      New_Line;
   end Set_Fg_RGB_Test;

   procedure Set_Bg_Test is
   begin
      New_Line;
      Put_Line ("-- Set_Bg_Test --");

      Put_Line (" Control:");
      New_Line;
      Put_Line ("  " & CSI & "40m  "
                     & CSI & "41m  "
                     & CSI & "42m  "
                     & CSI & "43m  "
                     & CSI & "44m  "
                     & CSI & "45m  "
                     & CSI & "46m  "
                     & CSI & "47m  "
                     & CSI & "49m  "
                     & CSI & "100m  "
                     & CSI & "101m  "
                     & CSI & "102m  "
                     & CSI & "103m  "
                     & CSI & "104m  "
                     & CSI & "105m  "
                     & CSI & "106m  "
                     & CSI & "107m  "
                     & CSI & "0m");

      Put_Line (" Set_Bg:");
      Put ("  ");
      for C in Color'Range loop
         Set_Bg (C);
         Put ("  ");
      end loop;
      Reset_All;
      New_Line;

      New_Line;
   end Set_Bg_Test;

   procedure Set_Bg_8_Test is
   begin
      New_Line;
      Put_Line ("-- Set_Bg_8_Test --");
      New_Line;

      for I in 0 .. 8 loop
         Put_Line (" Control:");
         Put ("  ");
         for J in 0 .. 31 loop
            Put (CSI & "48;5;");
            Put (I * 8 + J);
            Put ("m  ");
         end loop;
         Reset_All;
         New_Line;

         Put_Line (" Set_Bg:");
         Put ("  ");
         for J in 0 .. 31 loop
            Set_Bg (I * 8 + J);
            Put ("  ");
         end loop;
         Reset_All;
         New_Line;

         New_Line;
      end loop;
   end Set_Bg_8_Test;

   procedure Set_Bg_RGB_Test is
      C : constant Color_RGB := (245, 145, 45);
   begin
      New_Line;
      Put_Line ("-- Set_Bg_RGB_Test --");
      New_Line;

      Put_Line (" Control:");
      Put_Line ("  " & CSI & "48;2;245;145;45m  " & CSI & "0m");

      Put_Line (" Set_Bg:");
      Put ("  ");
      Set_Bg (C);
      Put ("  ");
      Reset_All;
      New_Line;

      New_Line;
   end Set_Bg_RGB_Test;

   procedure Set_Color_Test is
   begin
      New_Line;
      Put_Line ("-- Set_Color_Test --");
      New_Line;

      Put_Line ("Control:");
      Put_Line ("  " & CSI & "107;31mRed on Light Grey!" & CSI & "0m");

      Put_Line ("Set_Color:");
      Put ("  ");
      Set_Color (Red, B_White);
      Put ("Red on Light Grey!");
      Reset_All;
      New_Line;

      New_Line;
   end Set_Color_Test;

   procedure Set_Color_8_Test is
   begin
      New_Line;
      Put_Line ("-- Set_Color_8_Test --");
      New_Line;

      Put_Line ("Control:");
      Put_Line ("  " & CSI & "38;5;155;48;5;240mGreen on Grey!" & CSI & "0m");

      Put_Line ("Set_Color:");
      Put ("  ");
      Set_Color (155, 240);
      Put ("Green on Grey!");
      Reset_All;
      New_Line;

      New_Line;
   end Set_Color_8_Test;

   procedure Set_Color_RGB_Test is
      Fg : constant Color_RGB := (40, 200, 200);
      Bg : constant Color_RGB := (30, 30, 30);
   begin
      New_Line;
      Put_Line ("-- Set_Color_RGB_Test --");
      New_Line;

      Put_Line ("Control:");
      Put_Line ("  " & CSI & "38;2;40;200;200;48;2;30;30;30mTeal on Dark Grey!"
                     & CSI & "0m");

      Put_Line ("Set_Color:");
      Put ("  ");
      Set_Color (Fg, Bg);
      Put ("Teal on Dark Grey!");
      Reset_All;
      New_Line;

      New_Line;
   end Set_Color_RGB_Test;

   -------------------
   -- Styling Tests --
   -------------------

end Test.Ansi;
