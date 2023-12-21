using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Numerics;
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

    // Thinking out loud for part 2. For visited plots, we can summarize them in
    // two numbers, how many are reachable after an even and uneven number of
    // steps. But if we do just Dijkstra, what is the size of the frontier? With
    // 26501365 steps in the Manhattan metric, a "circle" is a diamond with
    // diagonals of size twice that, so a square with size of roughly 0.707
    // (= sqrt 0.5) times that, times 4 for 4 edges, so the frontier would be
    // at most 74,957,179 plots, which fits in memory just fine. We would need
    // to perform way more Dijkstra steps though -- is this going to finish in
    // time? I suspect it's not and we need to leverage the symmetry of the map,
    // but let's give it a try.
    static void Part2(string fname)
    {
      var grid = new List<String>();
      foreach (var line in File.ReadLines(fname))
      {
        // Since in part 2 the grid repeats infinitely, extend it to have a
        // power of two size, so we can use a bitmask instead of expensive
        // modulo. Perhaps this is a premature optimization ...
        var w = BitOperations.RoundUpToPowerOf2((uint)line.Length);
        grid.Add(line + line.Substring(0, (int)(w - line.Length)));
      }
      var h = BitOperations.RoundUpToPowerOf2((uint)grid.Count);
      grid.AddRange(grid.Slice(0, (int)(h - grid.Count)));

      var mask = (int)(h - 1);
      Console.WriteLine("Mask: {0}", mask);

      foreach (var line in grid) {
        Console.WriteLine(line);
      }

      var start = FindStart(grid);
      var frontier = new HashSet<Coord>();
      var prevFrontier = new HashSet<Coord>();
      frontier.Add(start);

      //int nSteps = 26501365;
      int nSteps = 5000;
      ulong[] counts = new ulong[2] { 1, 0 };

      for (int step = 1; step <= nSteps; step++)
      {
        // Empirical observation on the example input, when the frontier grows
        // a lot, it is usually by 120 or 130%, so size the hash set for the
        // next frontier to that so it doesn't need to reallocate.
        var expectedGrowth = frontier.Count * 166 / 128;
        var nextFrontier = new HashSet<Coord>(frontier.Count + expectedGrowth);

        foreach (Coord c in frontier)
        {
          Coord[] neighbors = {
            new Coord(c.X - 1, c.Y),
            new Coord(c.X + 1, c.Y),
            new Coord(c.X, c.Y - 1),
            new Coord(c.X, c.Y + 1),
          };
          foreach (Coord n in neighbors)
          {
            if (!prevFrontier.Contains(n) && grid[n.Y & mask][n.X & mask] != '#')
            {
              nextFrontier.Add(n);
            }
          }
        }
        counts[step & 1] += (ulong)nextFrontier.Count;

        if (nextFrontier.Count > frontier.Count + expectedGrowth)
        {
          Console.WriteLine
          (
              "Actual growth was {0}, about {1}%.",
              nextFrontier.Count - frontier.Count,
              100.0 * (float)nextFrontier.Count / (float)frontier.Count
          );
        }

        prevFrontier = frontier;
        frontier = nextFrontier;

        if (step <= 50 || step == 100 || step == 500 || step % 1000 == 0)
        {
          Console.WriteLine
          (
            "After step {0}, the frontier has size {1} and the count is {2}.",
            step, frontier.Count, counts[step & 1]
          );
        }
      }
    }

    static void Main(String[] args)
    {
      var fname = "example.txt";
      var grid = new List<String>();

      foreach (var line in File.ReadLines(fname))
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
      var frontier = new HashSet<Coord>();
      frontier.Add(start);

      int steps = 64;
      for (int step = 0; step < steps; step++)
      {
        frontier = frontier
          .SelectMany(Neighbors)
          .Where(c => grid[c.Y][c.X] != '#')
          .ToHashSet();
      }
      Console.WriteLine(String.Format("After {0} steps can reach {1} plots.", steps, frontier.Count));

      Part2(fname);
    }
  }
}
