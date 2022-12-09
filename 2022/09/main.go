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

func main() {
	data, err := os.ReadFile("example.txt")
	check(err)
	lines := strings.Split(string(data), "\n")
	for _, line := range lines {
    if line == "" {
      continue
    }
    direction := line[:1]
    distance, err := strconv.Atoi(line[2:])
    check(err)
		fmt.Printf("%s %d\n", direction, distance)
	}
}
