package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
  "math"
)

func check(e error) {
	if e != nil {
		panic(e)
	}
}

type Vec2 struct {
  x float64
  y float64
}

func (v Vec2) Norm() float64 {
  return math.Sqrt(v.x * v.x + v.y * v.y)
}

func (v Vec2) Normed() Vec2 {
  norm := v.Norm()
  return Vec2 { v.x / norm, v.y / norm }
}

func (v Vec2) Tangent() Vec2 {
  return Vec2 { -v.y, v.x }
}

func (v Vec2) Add(w Vec2) Vec2 {
  return Vec2 { v.x + w.x, v.y + w.y}
}

func (v Vec2) Sub(w Vec2) Vec2 {
  return Vec2 { v.x - w.x, v.y - w.y}
}

func (v Vec2) Mul(t float64) Vec2 {
  return Vec2 { v.x * t, v.y * t }
}

func (v Vec2) Dot(w Vec2) float64 {
  return v.x * w.x + v.y * w.y
}

type Hailstone struct {
	p [3]int
  v [3]int
}

func (h Hailstone) Pos2() Vec2 {
  x := float64(h.p[0])
  y := float64(h.p[1])
  return Vec2 { x, y }
}

func (h Hailstone) Vel2() Vec2 {
  x := float64(h.v[0])
  y := float64(h.v[1])
  return Vec2 { x, y }
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

// Return whether two hailstone trajectories intersect as defined in part 1.
func intersect2D(a Hailstone, b Hailstone) bool {
  va := a.Vel2().Normed()
  vb := b.Vel2().Normed()
  pa := a.Pos2()
  pb := b.Pos2()
  // Translate everything so pa is the origin.
  // We project the point (pb + tb * vb) onto the line defined by va.
  // On the line, we have:
  //    (pb + tb * vb) dot (va.Tangent) = 0
  // => (pb dot va.Tangent) + ((tb * vb) dot va.Tangent) = 0
  // => (pb dot va.Tangent) = -tb * (vb dot va.Tangent)
  // => tb = -(pb dot va.Tangent) / (vb dot va.Tangent)
  tb := -(pb.Sub(pa)).Dot(va.Tangent()) / (vb.Dot(va.Tangent()))
  ip := pa.Add(pb).Add(vb.Mul(tb))
	fmt.Printf("Got %f,%f hailstones\n", ip.x, ip.y)

  if (ip.x < 200000000000000.0) {
    return false;
  }
  if (ip.y < 200000000000000.0) {
    return false;
  }
  if (ip.x > 400000000000000.0) {
    return false;
  }
  if (ip.y > 400000000000000.0) {
    return false;
  }
  return true;
}

func main() {
  hailstones := loadInput("example.txt")
	fmt.Printf("Got %d hailstones\n", len(hailstones))
  for i, a := range hailstones {
    for j, b := range hailstones {
      if (j < i) {
        continue
      }
      intersect2D(a, b)
    }
  }
}
