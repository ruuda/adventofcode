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

type Vec3 struct {
  x float64
  y float64
  z float64
}

func (v Vec3) Add(w Vec3) Vec3 {
  return Vec3 { v.x + w.x, v.y + w.y, v.z + w.z}
}

func (v Vec3) Sub(w Vec3) Vec3 {
  return Vec3 { v.x - w.x, v.y - w.y, v.z - w.z}
}

func (v Vec3) Mul(t float64) Vec3 {
  return Vec3 { v.x * t, v.y * t, v.z * t}
}

func (v Vec3) Dot(w Vec3) float64 {
  return v.x * w.x + v.y * w.y + v.z * w.z
}

func (v Vec3) Cross(w Vec3) Vec3 {
  return Vec3 {
    v.y * w.z - v.z * w.y,
    v.z * w.x - v.x * w.z,
    v.x * w.y - v.y * w.x,
  }
}

func (v Vec3) Norm() float64 {
  return math.Sqrt(v.x * v.x + v.y * v.y + v.z * v.z)
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

func (h Hailstone) Pos3() Vec3 {
  x := float64(h.p[0])
  y := float64(h.p[1])
  z := float64(h.p[2])
  return Vec3 { x, y, z }
}

func (h Hailstone) Vel3() Vec3 {
  x := float64(h.v[0])
  y := float64(h.v[1])
  z := float64(h.v[2])
  return Vec3 { x, y, z }
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
  ta := -(pa.Sub(pb)).Dot(vb.Tangent()) / (va.Dot(vb.Tangent()))
  ip := pb.Add(vb.Mul(tb))
	fmt.Printf("Paths cross at %f,%f, @%f @%f \n", ip.x, ip.y, ta, tb)

  if (ip.x < 200000000000000.0) {
    return false
  }
  if (ip.y < 200000000000000.0) {
    return false
  }
  if (ip.x > 400000000000000.0) {
    return false
  }
  if (ip.y > 400000000000000.0) {
    return false
  }
  if (ta < 0.0) { return false }
  if (tb < 0.0) { return false }
  return true;
}

// Part 2:
// Observation: The ray we shoot, and the hailstone ray, lie in the same plane.
// Every ray defines a set of planes.
// The first one, we get to pick a position anywhere, but then the velocity is
// constrained to a plane. The next one, the velocity is constrained to a
// different plane, so it's unique. So with one guess and two hailstones, we
// determine the (normalized) velocity. Then we can compute the distance to a
// third hailstone ray, and minimize that over the starting position.
func part2(p Vec3, a Hailstone, b Hailstone, c Hailstone) float64 {
  // Two vectors in the plane of possible trajectories. The normal of that plane
  // is their cross product.
  av1 := a.Pos3().Sub(p)
  av2 := a.Pos3().Add(a.Vel3()).Sub(p)
  an := av1.Cross(av2)

  // We can do the same for the second plane.
  bv1 := b.Pos3().Sub(p)
  bv2 := b.Pos3().Add(b.Vel3()).Sub(p)
  bn := bv1.Cross(bv2)

  // The velocity then, is orthogonal to both planes' normals.
  v1 := an.Cross(bn)
  v2 := c.Vel3()
  p1 := p
  p2 := c.Pos3()
  n := v1.Cross(v2)
  n1 := v1.Cross(n)
  n2 := v2.Cross(n)

  // Compute the two nearest points on our trajectory and hailstone c
  // trajectory.
  c1 := v1.Mul(p2.Sub(p1).Dot(n2) / v1.Dot(n2)).Add(p1)
  c2 := v2.Mul(p1.Sub(p2).Dot(n1) / v2.Dot(n1)).Add(p2)
  return c1.Sub(c2).Norm()
}

func main() {
  hailstones := loadInput("example.txt")
	fmt.Printf("Got %d hailstones\n", len(hailstones))
  var n = 0
  for i, a := range hailstones {
    for j, b := range hailstones {
      if (j <= i) {
        continue
      }
      fmt.Printf("Hailstone A: %d, %d @ %d, %d\n", a.p[0], a.p[1], a.v[0], a.v[1])
      fmt.Printf("Hailstone B: %d, %d @ %d, %d\n", b.p[0], b.p[1], b.v[0], b.v[1])
      if (intersect2D(a, b)) {
        n += 1
      }
      fmt.Printf("\n")
    }
  }
  fmt.Printf("%d intersections in test area.\n", n)

  fmt.Printf("Part 2\n")
  var p = Vec3 { 24.0, 13.0, 0.0 }
  x := Vec3 { 1.0, 0.0, 0.0 }
  y := Vec3 { 0.0, 1.0, 0.0 }
  z := Vec3 { 0.0, 0.0, 1.0 }
  var d = 1e9;
  for d > 1e-5 {
    // In principle we need only the first three stones, but we can put all of
    // them in as a sanity check.
    for i, _ := range hailstones {
      j := (i + 1) % len(hailstones)
      k := (i + 2) % len(hailstones)

      d = part2(p, hailstones[i], hailstones[j], hailstones[k])
      ddx := part2(p.Add(x), hailstones[i], hailstones[j], hailstones[k]) - d
      ddy := part2(p.Add(y), hailstones[i], hailstones[j], hailstones[k]) - d
      ddz := part2(p.Add(z), hailstones[i], hailstones[j], hailstones[k]) - d
      fmt.Printf(
        "d=%f, p=%f,%f,%f, dd/dp=%f,%f,%f\n",
        d, p.x, p.y, p.z, ddx, ddy, ddz,
      )
      p.x -= ddx * 10.0
      p.y -= ddy * 10.0
      p.z -= ddz * 10.0
    }
  }
}
