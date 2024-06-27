with Ada.Text_IO; use Ada.Text_IO;

with Test.Ansi; use Test.Ansi;
with Test.User_IO; use Test.User_IO;

procedure Tests is
begin
   Put_Line ("IO_Utils Testing Suite");
   New_Line;

   Coloring_Tests;
   User_IO_Tests;
end Tests;
