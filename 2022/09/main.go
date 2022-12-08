package main

import (
	"os"
	"fmt"
)

func check(e error) {
	if e != nil {
		panic(e)
	}
}

func main() {
	dat, err := os.ReadFile("example.txt")
	check(err)
	fmt.Printf("%s\n", dat)
}