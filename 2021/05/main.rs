use std::cmp;
use std::collections::HashSet;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::io;
use std::str::FromStr;

/// A line segment defined by a direction vector (dx, dy), a starting point
/// (x, y) on the line, and the length of the segment.
///
/// Note, the direction vector does not need to be a unit vector, but for the
/// purpose of checking intersections, we do assume that all directions have one
/// unique representation.
#[derive(Debug)]
struct Line {
    x: i32,
    y: i32,
    dx: i32,
    dy: i32,
    len: i32,
}

impl Line {
    fn intersect(&self, other: &Line, out: &mut HashSet<(i32, i32)>) {
        let is_parallel =
            (self.dx * other.dx + self.dy * other.dy).abs() ==
            (self.dx.abs() + self.dy.abs());

        if is_parallel {
            // Case 1: The lines are parallel. Check if they overlap.
            let perp0 = self.dy * self.x - self.dx * self.y;
            let perp1 = other.dy * other.x - other.dx * other.y;
            if perp0 != perp1 {
                return // The lines do not overlap.
            }

            // The lines do overlap, but do the line segments overlap?
            let par0 = self.dx * self.x + self.dy * self.y;
            let par1 = other.dx * other.x + other.dy * other.y;
            let t0 = cmp::max(par0, par1);
            let t1 = cmp::min(par0 + self.len, par1 + other.len);
            for t in t0..t1 {
                let x = self.dx * t + self.dy * perp0;
                let y = self.dy * t - self.dx * perp0;
                out.insert((x, y));
            }
        } else {
            // Case 2: The lines are not parallel, therefore they intersect in
            // one unique point. (Which may not lie inside the line segment
            // though).

            // If we translate everything such that `self.xy` is at (0, 0), and
            // then parametrize `other` as `Q(t) = other.xy + t * other.dxy`,
            // then the point at which `dot(Q(t), normal(self.xy)) = 0` is the
            // point where the two lines (but maybe not line segments)
            // intersect. We can solve for `t`, and if it lies within other's
            // length, the intersection lies at least on that line segement.

            // Q(t1) = (b.x + b.dx * t1).
            //    dot(Q(t1), normal(a.dx)) = 0
            // => dot(b.x + b.dx * t1, na) = 0
            // => dot(b.x, na) + t1 * dot(b.dx, na) = 0
            // t1 = - dot(b.x, na) / dot(b.dx, na)

            let px = other.x - self.x;
            let py = other.y - self.y;
            let dot_x = py * self.dx - px * self.dy;
            let dot_dx = other.dy * self.dx - other.dx * self.dy;
            let t = -dot_x / dot_dx;
            if t < 0 || t > other.len {
                // Intersection does not lie on this line segement.
                return
            }

            let x = other.x + other.dx * t;
            let y = other.y + other.dy * t;

            // Now we flip the role of `self` and `other` and do the same.
            let px = self.x - other.x;
            let py = self.y - other.y;
            let dot_x = py * other.dx - px * other.dy;
            let dot_dx = self.dy * other.dx - self.dx * other.dy;
            let t = -dot_x / dot_dx;
            if t < 0 || t > self.len {
                // Intersection does not lie on this line segement.
                return
            }

            debug_assert_eq!(x, self.x + self.dx * t);
            debug_assert_eq!(y, self.y + self.dy * t);

            out.insert((x, y));
        }
    }
}

fn main() -> io::Result<()> {
    let mut lines = Vec::new();

    for file_line_io in BufReader::new(File::open("test.txt")?).lines() {
        let file_line = file_line_io?;

        let mut coords = file_line.split(" -> ");
        let mut c0 = coords
            .next()
            .expect("Need two coords separated by ' -> '.")
            .split(",");
        let mut c1 = coords
            .next()
            .expect("Need two coords separated by ' -> '.")
            .split(",");
        let x0 = i32::from_str(c0.next().expect("Syntax error")).expect("Need integer coordinates");
        let y0 = i32::from_str(c0.next().expect("Syntax error")).expect("Need integer coordinates");
        let x1 = i32::from_str(c1.next().expect("Syntax error")).expect("Need integer coordinates");
        let y1 = i32::from_str(c1.next().expect("Syntax error")).expect("Need integer coordinates");

        /*if x0 > x1 {
            std::mem::swap(&mut x0, &mut x1);
            std::mem::swap(&mut y0, &mut y1);
        }*/
        let dx = (x1 - x0).signum();
        let dy = (y1 - y0).signum();

        let z = if dx.abs() + dy.abs() > 0 {
            Line {
                x: x0, y: y0,
                dx, dy,
                len: 1 + ((y1 - y0).abs() + (x1 - x0).abs()) / (dx.abs() + dy.abs())
            }
        } else {
            // If it's one cell, it doesn't have any direction in particular,
            // pick one.
            Line {
                x: x0, y: y0,
                dx: 1, dy: 0,
                len: 1,
            }
        };
        lines.push(z);
    }
    for z in lines.iter() {
        println!("{:?}", z);
    }

    let mut intersections = HashSet::new();

    for (i, z) in lines.iter().enumerate() {
        for (_, w) in lines[i + 1..].iter().enumerate() {
            z.intersect(&w, &mut intersections);
        }
    }
    let mut is: Vec<_> = intersections.iter().collect();
    is.sort();
    println!("{:?}", is);
    println!("{}", intersections.len());

    Ok(())
}
