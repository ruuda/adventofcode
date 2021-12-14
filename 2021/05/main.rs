use std::cmp;
use std::collections::HashSet;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::io;
use std::str::FromStr;

/// An axis-aligned line.
#[derive(Copy, Clone, Debug)]
struct AaLine { x: u32, y: u32, len: u32 }

fn report_horizontal_intersections<X: Fn(AaLine) -> u32, Y: Fn(AaLine) -> u32>(
    lines: &[AaLine],
    px: X,
    py: Y,
) -> HashSet<(u32, u32)> {
    let mut out = HashSet::new();

    'outer: for i in 0..lines.len() {
        for j in (i + 1)..lines.len() {
            let z0 = lines[i];
            let z1 = lines[j];

            if py(z0) != py(z1) {
                // These lines are not at the same y-coordinate, they surely do
                // not overlap.
                continue 'outer;
            }

            if px(z1) <= px(z0) + z0.len {
                // The lines overlap, and z0 is left of z1. The reverse case
                // does not happen, because our input is sorted.
                for x in px(z1)..cmp::min(px(z0) + z0.len, px(z1) + z1.len) {
                    out.insert((x, py(z0)));
                }
            }
        }
    }

    out
}

/// Report intersections between horizontal and vertical lines.
fn report_intersections(hlines: &[AaLine], vlines: &[AaLine], out: &mut HashSet<(u32, u32)>) {
    for zh in hlines {
        for zv in vlines {
            // We could take advantage of the vlines being sorted to early out
            // here, but for now quadratic complexity is acceptable.
            if zh.y < zv.y || zh.y >= zv.y + zv.len {
                continue
            }
            if zv.x < zh.x || zv.x >= zh.x + zh.len {
                continue
            }
            out.insert((zv.x, zh.y));
        }
    }
}

fn main() -> io::Result<()> {
    let mut hlines = Vec::new();
    let mut vlines = Vec::new();

    for file_line_io in BufReader::new(File::open("input.txt")?).lines() {
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
                vlines.push(AaLine { x: x0, y: y0, len: 1 + y1 - y0 });
            } else {
                vlines.push(AaLine { x: x0, y: y1, len: 1 + y0 - y1 });
            }
        } else if y0 == y1 {
            if x1 > x0 {
                hlines.push(AaLine { x: x0, y: y0, len: 1 + x1 - x0 });
            } else {
                hlines.push(AaLine { x: x1, y: y0, len: 1 + x0 - x1 });
            }
        } else {
            // We ignore non-axis-aligned lines in part 1.
        }
    }

    // Sort the lines so we can efficiently find overlap.
    hlines.sort_by_key(|z| (z.y, z.x, z.len));
    vlines.sort_by_key(|z| (z.x, z.y, z.len));

    // Start by computing direct overlap between the lines. The function is
    // written from the horizontal point of view, for vertical we swap the
    // coordinates.
    let h_isects = report_horizontal_intersections(&hlines[..], |z| z.x, |z| z.y);
    let v_isects = report_horizontal_intersections(&vlines[..], |z| z.y, |z| z.x);

    // Take the union of the two sets of points. Swap back the x and y for the
    // vertical intersections.
    let mut isects = h_isects;
    for (y, x) in v_isects {
        isects.insert((x, y));
    }

    // Now aside from the intersections between lines of the same orientation,
    // find intersections between different orientations.
    report_intersections(&hlines[..], &vlines[..], &mut isects);

    println!("{}", isects.len());

    Ok(())
}
