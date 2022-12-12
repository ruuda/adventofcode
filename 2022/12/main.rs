use std::fs;
use std::io;
use std::collections::{HashMap, BinaryHeap};

fn main() -> io::Result<()> {
    // Parse the field input into a 2d grid. Conveniently the input is ascii,
    // so we can just treat the letters as 8-bit height values.
    let raw_field = fs::read_to_string("example.txt")?;
    let width = raw_field.find('\n').expect("Input must contain newline.");
    let field: Vec<&[u8]> = raw_field
        .as_bytes()
        .chunks(width + 1)
        .map(|line| &line[..width])
        .collect();
    let height = field.len();

    // The start and end need special treatment to get their elevation.
    let get_z = |(x, y): (i32, i32)| match field[y as usize][x as usize] {
        b'E' => b'z',
        b'S' => b'a',
        z => z,
    };

    // Locate the start and end position in the grid.
    let mut start = (0, 0);
    let mut end = (0, 0);
    for (y, line) in field.iter().enumerate() {
        for (x, ch) in line.iter().enumerate() {
            match ch {
                b'S' => start = (x as i32, y as i32),
                b'E' => end = (x as i32, y as i32),
                _ => continue,
            }
        }
    }

    // Apply Dijkstra, we keep a set of open nodes ordered by distance from the
    // start, and a set of closed nodes, with the minimum distance to reach
    // them.
    let mut open = BinaryHeap::new();
    let mut closed: HashMap<(i32, i32), u32> = HashMap::new();
    open.push((0, start));

    loop {
        let (dist, p) = open.pop().expect("We should have found the exit.");
        let (x, y) = p;
        let zp = get_z(p);

        if p == end {
            println!("Shortest route to exit: {} steps", dist);
            break;
        }
        closed.insert(p, dist);

        for (dx, dy) in [(-1, 0), (1, 0), (0, -1), (0, 1)] {
            let (cx, cy) = (x + dx, y + dy);
            if cx < 0 || cx >= width as i32 || cy < 0 || cy >= height as i32 {
                // Cell is outside of our height field.
                continue;
            }

            let c = (cx, cy);
            let zc = get_z(c);

            if closed.contains_key(&c) {
                // We already have a shorter route to this cell.
                continue;
            }

            if zc > zp + 1 {
                // We cannot step onto this cell, it's too high.
                continue;
            }

            open.push((dist + 1, c));
        }
    }

    Ok(())
}
