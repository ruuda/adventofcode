use std::collections::{BinaryHeap, HashSet};
use std::fs;
use std::io;

/// Return the length of the shortest path from a cell that satisfies the
/// `is_start` condition, to `end`.
pub fn dijkstra_until<F: Fn(u8) -> bool>(field: &[&[u8]], end: (i32, i32), is_start: F) -> i32 {
    let height = field.len();
    let width = field[0].len();

    // The start and end need special treatment to get their elevation.
    let get_z = |(x, y): (i32, i32)| match field[y as usize][x as usize] {
        b'E' => b'z',
        b'S' => b'a',
        z => z,
    };

    // Apply Dijkstra, we keep a set of open nodes ordered by distance from the
    // start, and a set of closed nodes, with the minimum distance to reach
    // them.
    let mut open = BinaryHeap::new();
    let mut closed: HashSet<(i32, i32)> = HashSet::new();
    open.push((0, end));

    loop {
        // We store the negative distance, because Rust's BinaryHeap is a
        // max-heap, not a min-heap.
        let (neg_dist, p) = open.pop().expect("We should have found the exit.");
        let (x, y) = p;
        let zp = get_z(p);

        if is_start(field[y as usize][x as usize]) {
            return -neg_dist;
        }

        if closed.contains(&p) {
            continue;
        }

        closed.insert(p);

        for (dx, dy) in [(-1, 0), (1, 0), (0, -1), (0, 1)] {
            let (cx, cy) = (x + dx, y + dy);
            if cx < 0 || cx >= width as i32 || cy < 0 || cy >= height as i32 {
                // Cell is outside of our height field.
                continue;
            }

            let c = (cx, cy);
            let zc = get_z(c);

            if zp > zc + 1 {
                // We cannot step from this cell, we are too high.
                continue;
            }

            open.push((neg_dist - 1, c));
        }
    }
}

fn main() -> io::Result<()> {
    // Parse the field input into a 2d grid. Conveniently the input is ascii,
    // so we can just treat the letters as 8-bit height values.
    let raw_field = fs::read_to_string("input.txt")?;
    let width = raw_field.find('\n').expect("Input must contain newline.");
    let field: Vec<&[u8]> = raw_field
        .as_bytes()
        .chunks(width + 1)
        .map(|line| &line[..width])
        .collect();

    // Locate the end position in the grid.
    let mut end = (0, 0);
    for (y, line) in field.iter().enumerate() {
        for (x, &ch) in line.iter().enumerate() {
            if ch == b'E' {
                end = (x as i32, y as i32);
            }
        }
    }

    let n_steps = dijkstra_until(&field, end, |ch| ch == b'S');
    println!("Part 1: Shortest route from S to E: {}", n_steps);

    let n_steps = dijkstra_until(&field, end, |ch| ch == b'S' || ch == b'a');
    println!("Part 2: Shortest route from any a to E: {}", n_steps);

    Ok(())
}
