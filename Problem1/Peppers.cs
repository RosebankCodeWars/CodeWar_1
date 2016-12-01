using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace Q1
{
  public class Program
  {
    public static void Main(string[] args)
    {
      var quantity = int.Parse(args[0]);

      if (quantity < 6)
      {
        Console.WriteLine("At least 6!");
        return;
      }

      CalcPacks(quantity);
    }

    public static void CalcPacks(int quantity)
    {
      var minPacks = int.MaxValue;

      for (int i_13 = quantity / 13; i_13 >= 0; i_13--)
      {
        var q_13 = i_13 * 13;

        for (int i_11 = (quantity - q_13) / 11; i_11 >= 0; i_11--)
        {
          var q_11 = i_11 * 11;

          for (int i_6 = (quantity - q_13 - q_11) / 6; i_6 >= 0; i_6--)
          {
            var q_6 = i_6 * 6;

            if (q_13 + q_11 + q_6 == quantity
              && i_13 + i_11 + i_6 < minPacks)
            {
              minPacks = i_6 + i_11 + i_13;
              Console.WriteLine($"13:{i_13} 11:{i_11} 6:{i_6}");
            }
          }
        }
      }
    }
  }
}
