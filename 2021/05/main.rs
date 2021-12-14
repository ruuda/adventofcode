use std::fs::File;
use std::io::{BufRead, BufReader};
use std::io;
use std::str::FromStr;

/// An axis-aligned line.
#[derive(Debug)]
struct AaLine { x: u32, y: u32, len: u32 }

fn main() -> io::Result<()> {
    let mut hlines = Vec::new();
    let mut vlines = Vec::new();

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
        let x0 = u32::from_str(c0.next().expect("Need a coordinate lin 'x,y' format.")).expect("Need integer coordinates");
        let y0 = u32::from_str(c0.next().expect("Need a coordinate lin 'x,y' format.")).expect("Need integer coordinates");
        let x1 = u32::from_str(c1.next().expect("Need a coordinate lin 'x,y' format.")).expect("Need integer coordinates");
        let y1 = u32::from_str(c1.next().expect("Need a coordinate lin 'x,y' format.")).expect("Need integer coordinates");

        if x0 == x1 {
            if y1 > y0 {
                vlines.push(AaLine { x: x0, y: y0, len: y1 - y0 });
            } else {
                vlines.push(AaLine { x: x0, y: y1, len: y0 - y1 });
            }
        } else if y0 == y1 {
            if x1 > x0 {
                hlines.push(AaLine { x: x0, y: y0, len: x1 - x0 });
            } else {
                hlines.push(AaLine { x: x1, y: y0, len: x0 - x1 });
            }
        } else {
            // We ignore non-axis-aligned lines in part 1.
        }

        for z in hlines.iter() {
            println!("{:?}", z);
        }
    }
    Ok(())
}
