using System;
using System.IO;

void Main()
{
  var lines = File.ReadLines("example.txt");
  foreach (var line in lines)
  {
    Console.WriteLine(String.Format("Hello {0}", line));
  }
}

Main();
