using System.Collections.Generic;
using System.IO;
using System;

namespace App
{
  struct Coord {
    public int X { get; }
    public int Y { get; }

    public Coord(int x, int y)
    {
      X = x;
      Y = y;
    }
  }

  internal class Program
  {
    static IEnumerable<Coord> Neighbors(Coord c)
    {
      yield return new Coord(c.X - 1, c.Y);
      yield return new Coord(c.X + 1, c.Y);
      yield return new Coord(c.X, c.Y - 1);
      yield return new Coord(c.X, c.Y + 1);
    }

    static Coord FindStart(List<String> grid)
    {
      for (int y = 0; y < grid.Count; y++)
      {
        var line = grid[y];
        for (int x = 0; x < line.Length; x++)
        {
          if (line[x] == 'S') return new Coord(x, y);
        }
      }
      throw new Exception("There should be an 'S' on the grid.");
    }

    static void Main(String[] args)
    {
      var grid = new List<String>();

      foreach (var line in File.ReadLines("example.txt"))
      {
        // Add walls around the grid so we don't have to worry about index out of
        // bounds.
        grid.Add("#" + line + "#");
      }

      // Add walls before and after too.
      var pad = new String('#', grid[0].Length);
      grid.Insert(0, pad);
      grid.Add(pad);

      foreach (var line in grid) {
        Console.WriteLine(line);
      }

      var start = FindStart(grid);
    }
  }
}
