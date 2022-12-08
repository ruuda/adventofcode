package main

import (
	"os"
	"fmt"
	"strings"
)

func check(e error) {
	if e != nil {
		panic(e)
	}
}

func main() {
	data, err := os.ReadFile("example.txt")
	lines := strings.Split(string(data), "\n")
	check(err)
	for _, line := range lines {
		fmt.Printf("%s\n", line)
	}
}
