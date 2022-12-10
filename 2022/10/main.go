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

func main() {
	data, err := os.ReadFile("example2.txt")
	check(err)
	lines := strings.Split(string(data), "\n")

	var pc int = 0
	var x int = 1

	for _, line := range lines {
		if line == "" {
			continue
		}
		parts := strings.Split(line, " ")
		switch parts[0] {
		case "noop":
			pc += 1
			fmt.Printf("noop\t\tpc=%d x=%d\n", pc, x)
		case "addx":
			delta, err := strconv.Atoi(parts[1])
			check(err)
			pc += 2
			x += delta
			fmt.Printf("addx %d\t\tpc=%d x=%d\n", delta, pc, x)
		}
	}
}
