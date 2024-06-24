with Ada.Text_IO; use Ada.Text_IO;

with Test.Ansi; use Test.Ansi;

procedure Tests is
begin
   Put_Line ("IO_Utils Testing Suite");
   New_Line;

   Coloring_Tests;
end Tests;
