package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

func check(e error) {
	if e != nil {
		panic(e)
	}
}

type Coord struct {
  x int
  y int
}

func loadCave() [][]Coord {
	var cave = make([][]Coord, 0)

	data, err := os.ReadFile("example.txt")
	check(err)
	lines := strings.Split(string(data), "\n")

	for _, line := range lines {
		if line == "" {
			continue
		}
    var vertices = make([]Coord, 0)
		coords := strings.Split(line, " -> ")
    for _, coord := range coords {
      xy := strings.Split(coord, ",")
			x, err := strconv.Atoi(xy[0])
      check(err)
			y, err := strconv.Atoi(xy[1])
      vertices = append(vertices, Coord{x, y})
    }
    cave = append(cave, vertices)
  }
  return cave
}

func signum(x int) int {
  if x < 0 {
    return -1;
  } else if x > 0 {
    return 1;
  } else {
    return 0;
  }
}

// Return the cave (a cell is 0 for air, 1 for rock) and the x-offset.
func drawCave() ([][]int, int) {
	caveVertices := loadCave()
  var minX = caveVertices[0][0].x
  var maxX = caveVertices[0][0].x
  var maxY = caveVertices[0][0].y
  for _, verts := range caveVertices {
    for _, coord := range verts {
      if coord.x < minX { minX = coord.x; }
      if coord.x > maxX { maxX = coord.x; }
      if coord.y > maxY { maxY = coord.y; }
    }
  }
  w := 1 + maxX - minX;
  h := 1 + maxY;
  cave := make([][]int, h);
  for y := 0; y < h; y++ {
    cave[y] = make([]int, w);
  }

  for _, verts := range caveVertices {
    p := verts[0];
    var x, y int;
    for _, q := range verts[1:] {
      dx := signum(q.x - p.x);
      dy := signum(q.y - p.y);
      if dx != 0 && dy != 0 {
        fmt.Printf("%d,%d -> %d,%d\n", p.x, p.y, q.x, q.y);
        panic("Did not expect diagonal line.");
      }
      for x, y = p.x, p.y; x != q.x || y != q.y; {
        cave[y][x - minX] = 1;
        x += dx;
        y += dy;
      }
      p = q;
    }
  }

  return cave, minX;
}

func main() {
  cave, _ := drawCave();
  for _, line := range cave {
    for _, char := range line {
      switch char {
      case 0:
        fmt.Printf(" ");
      case 1:
        fmt.Printf("#");
      case 2:
        fmt.Printf("o");
      }
    }
    fmt.Printf("\n");
  }
}
