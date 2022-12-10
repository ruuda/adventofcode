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

	var crtX = 0
	fmt.Printf("Part 2, display output:\n\n")

	for _, instr := range instructions {
		pc += instr.latency

		// For part 2, we need to figure out whether the CRT beam hit the pixel.
		for i := 0; i < instr.latency; i++ {
			if crtX >= x-1 && crtX <= x+1 {
				fmt.Printf("#")
			} else {
				fmt.Printf(" ")
			}
			crtX++
			if crtX >= 40 {
				fmt.Printf("\n")
				crtX -= 40
			}
		}

		// For part 1, we need to count the total "signal strength"
		if pc >= nextSamplePc {
			signalStrength := x * nextSamplePc
			// Instructions say to find the signal strength up to the 220th cycle.
			if nextSamplePc <= 220 {
				totalSignalStrength += signalStrength
			}
			nextSamplePc += 40
		}

		x += instr.deltaX
	}

	fmt.Printf("\nPart 1, total signal strength: %d\n", totalSignalStrength)
}
