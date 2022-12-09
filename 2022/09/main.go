package main

import (
	"os"
	"fmt"
	"strings"
  "strconv"
)

func check(e error) {
	if e != nil {
		panic(e)
	}
}

type Point struct {
  x, y int
}

// Lol no generics, how is this not in the stdlib?
func absInt(x int) int {
  if x < 0 {
    return -x
  } else {
    return x
  }
}

func sgnInt(x int) int {
  if x < 0 {
    return -1
  } else if x > 0 {
    return 1
  } else {
    return 0
  }
}

func main() {
	data, err := os.ReadFile("input.txt")
	check(err)
	lines := strings.Split(string(data), "\n")

  headPos := Point{0, 0}
  tailPos := Point{0, 0}
  
  // Lol, no built-in set data structure.
  tailPositions := map[Point]int{tailPos: 1}

	for _, line := range lines {
    if line == "" {
      continue
    }
    direction := line[:1]
    distance, err := strconv.Atoi(line[2:])
    check(err)

    for i := 0; i < distance; i++ {
      // Move the head.
      switch direction {
      case "U":
        headPos.y += 1
      case "D":
        headPos.y -= 1
      case "R":
        headPos.x += 1
      case "L":
        headPos.x -= 1
      }

      // The tail moves diagonally if it is not within distance 1 of the head.
      dx := headPos.x - tailPos.x
      dy := headPos.y - tailPos.y

      if absInt(dx) <= 1 && absInt(dy) <= 1 {
        // Don't move, the head and tail are already touching.
      } else if absInt(dx) == 0 {
        // Already in the same column, move only within the column.
        tailPos.y += sgnInt(dy)
      } else if absInt(dy) == 0 {
        // ALready in the same row, move only within the row.
        tailPos.x += sgnInt(dx)
      } else {
        // We need to do a diagonal move.
        tailPos.x += sgnInt(dx)
        tailPos.y += sgnInt(dy)
      }

      tailPositions[tailPos] = 1

		fmt.Printf(
      "%s %d/%d -> H(%d, %d) T(%d, %d), %d unique positions\n",
      direction, i, distance,
      headPos.x, headPos.y,
      tailPos.x, tailPos.y,
      len(tailPositions),
    )
    }
	}
}
