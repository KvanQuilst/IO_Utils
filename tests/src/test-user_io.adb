with Ada.Text_IO; use Ada.Text_IO;
with AUnit.Assertions; use AUnit.Assertions;

with IO_Utils.User_IO; use IO_Utils.User_IO;

package body Test.User_IO is

   procedure User_IO_Tests is
   begin
      Put_Line ("-------------------");
      Put_Line ("-- User_IO Tests --");
      Put_Line ("-------------------");
      Put_Line ("These tests require user input.");

      Get_Integer_Test_1;
      Get_Integer_Test_2;

   end User_IO_Tests;

   procedure Get_Integer_Test_1 is
      Val    : Integer range 0 .. 9;
   begin
      New_Line;
      Put_Line ("-- Get_Integer_Test_1 --");
      New_Line;

      Put_Line ("Enter the number: " & TEST_VAL_1'Image);
      Val := Get_Integer (TEST_VAL_1, TEST_VAL_1);
      New_Line;

      Assert (Val = TEST_VAL_1, "Val /= Target");

      New_Line;
   end Get_Integer_Test_1;

   procedure Get_Integer_Test_2 is
      Val    : Integer range -9 .. 0;
   begin
      New_Line;
      Put_Line ("-- Get_Integer_Test_2 --");
      New_Line;

      Put_Line ("Enter the number: " & TEST_VAL_2'Image);
      Val := Get_Integer (TEST_VAL_2, TEST_VAL_2);
      New_Line;

      Assert (Val = TEST_VAL_2, "Val /= Target");

      New_Line;
   end Get_Integer_Test_2;

end Test.User_IO;
