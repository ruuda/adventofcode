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

type Hailstone struct {
	p [3]int
  v [3]int
}

func loadInput(fname string) []Hailstone {
	var hailstones = make([]Hailstone, 0)

	data, err := os.ReadFile(fname)
	check(err)
	lines := strings.Split(string(data), "\n")

	for _, line := range lines {
		if line == "" {
			continue
		}
    var h = Hailstone{};
    pv := strings.Split(line, " @ ")
    ps := strings.Split(pv[0], ", ")
    vs := strings.Split(pv[1], ", ")
		for i, c := range ps {
			x, err := strconv.Atoi(strings.TrimSpace(c))
			check(err)
      h.p[i] = x
    }
		for i, c := range vs {
			x, err := strconv.Atoi(strings.TrimSpace(c))
			check(err)
      h.v[i] = x
    }
    hailstones = append(hailstones, h)
	}

	return hailstones
}

func main() {
  hailstones := loadInput("example.txt")
	fmt.Printf("Got %d hailstones\n", len(hailstones))
}

