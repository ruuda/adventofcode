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

	data, err := os.ReadFile("input.txt")
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
		return -1
	} else if x > 0 {
		return 1
	} else {
		return 0
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
			if coord.x < minX {
				minX = coord.x
			}
			if coord.x > maxX {
				maxX = coord.x
			}
			if coord.y > maxY {
				maxY = coord.y
			}
		}
	}
	w := 1 + maxX - minX
	h := 1 + maxY
	cave := make([][]int, h)
	for y := 0; y < h; y++ {
		cave[y] = make([]int, w)
	}

	for _, verts := range caveVertices {
		p := verts[0]
		var x, y int
		for _, q := range verts[1:] {
			dx := signum(q.x - p.x)
			dy := signum(q.y - p.y)
			if dx != 0 && dy != 0 {
				fmt.Printf("%d,%d -> %d,%d\n", p.x, p.y, q.x, q.y)
				panic("Did not expect diagonal line.")
			}
			cave[p.y][p.x-minX] = 1
			for x, y = p.x, p.y; x != q.x || y != q.y; {
				x += dx
				y += dy
				cave[y][x-minX] = 1
			}
			p = q
		}
	}

	return cave, minX
}

func printCave(cave [][]int) {
	for _, line := range cave {
		for _, char := range line {
			switch char {
			case 0:
				fmt.Printf(" ")
			case 1:
				fmt.Printf("#")
			case 2:
				fmt.Printf(".")
			}
		}
		fmt.Printf("\n")
	}
}

func dropSand(cave [][]int, x0 int) bool {
	w, h := len(cave[0]), len(cave)
	var x = x0
	var y = 0

	if cave[y][x] != 0 {
		// The starting position contains sand, we can drop no more sand.
		return false
	}

	for {
		if y+1 == h {
			// Would fall off the world at the bottom.
			return false
		}
		if cave[y+1][x] == 0 {
			y += 1
			continue
		}
		if x == 0 {
			// Would fall off the world at the left.
			return false
		}
		if cave[y+1][x-1] == 0 {
			y += 1
			x -= 1
			continue
		}
		if x+1 == w {
			// Would fall off the world at the right.
			return false
		}
		if cave[y+1][x+1] == 0 {
			y += 1
			x += 1
			continue
		}
		// The sand can't move anywhere else, it comes to rest here.
		// We mark sand with 2 (1 is rock, 0 is air).
		cave[y][x] = 2
		return true
	}
}

func main() {
	cave, xoff := drawCave()
	var nSand = 0
	for i := 1; ; i++ {
		fmt.Printf("\nIteration %d:\n", i)
		printCave(cave)
		if dropSand(cave, 500-xoff) {
			nSand++
		} else {
			break
		}
	}

	fmt.Printf("Part 1, amount of sand: %d\n", nSand)
}
