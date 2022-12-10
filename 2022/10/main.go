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

type Instr struct {
	latency int
	deltaX  int
}

func loadProgram() []Instr {
	instructions := make([]Instr, 0)

	data, err := os.ReadFile("input.txt")
	check(err)
	lines := strings.Split(string(data), "\n")

	for _, line := range lines {
		if line == "" {
			continue
		}
		parts := strings.Split(line, " ")
		switch parts[0] {
		case "noop":
			instr := Instr{1, 0}
			instructions = append(instructions, instr)
		case "addx":
			delta, err := strconv.Atoi(parts[1])
			check(err)
			instr := Instr{2, delta}
			instructions = append(instructions, instr)
		}
	}

	return instructions
}

func main() {
	instructions := loadProgram()

	var pc = 0
	var x = 1

	// Next value of the program counter where we need to sample the X register.
	var nextSamplePc = 20
	var totalSignalStrength = 0

	for _, instr := range instructions {
		pc += instr.latency

		if pc >= nextSamplePc {
			signalStrength := x * nextSamplePc
			fmt.Printf("pc=%d\tx=%d\tss=%d\n", nextSamplePc, x, signalStrength)
			// Instructions say to find the signal strength up to the 220th cycle.
			if nextSamplePc <= 220 {
				totalSignalStrength += signalStrength
			}
			nextSamplePc += 40
		}

		x += instr.deltaX
	}

	fmt.Printf("Part 1, total signal strength: %d\n", totalSignalStrength)
}
