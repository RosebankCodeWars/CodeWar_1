using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace CodeWars
{
  public class Program
  {
    public static void Main(string[] args)
    {
      var size = int.Parse(args[0]);
      if (size % 2 != 0) { throw new Exception("Size must be even!"); }

      var rows = int.Parse(args[1]);
      var cols = int.Parse(args[2]);

      GenDiamond(size, cols, rows);
    }

    public static void GenDiamond(int size, int cols, int rows)
    {
      var half = size / 2;

      for (int row = 0; row < rows; row++)
      {
        for (int line = 0; line < half; line++)
        {
          for (int col = 0; col < cols; col++)
          {
            GenDiamondLine(half, line, true);
          }
          Console.WriteLine();
        }

        for (int line = half - 1; line >= 0; line--)
        {
          for (int col = 0; col < cols; col++)
          {
            GenDiamondLine(half, line, false);
          }
          Console.WriteLine();
        }
      }
    }

    public static void GenDiamondLine(int half, int line, bool top)
    {
      for (int h1 = 0; h1 < half - 1 - line; h1++)
      {
        Console.Write("#");
      }
      for (int l = 0; l < line; l++)
      {
        Console.Write(top ? "/" : "\\");
      }
      for (int r = 0; r < line; r++)
      {
        Console.Write(top ? "\\" : "/");
      }
      for (int h2 = 0; h2 < half - 1 - line; h2++)
      {
        Console.Write("#");
      }
    }
  }
}

