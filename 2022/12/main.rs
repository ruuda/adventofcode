use std::fs;
use std::io;

fn main() -> io::Result<()> {
    let raw_field = fs::read_to_string("example.txt")?;
    let width = raw_field.find('\n').expect("Input must contain newline.");
    let field: Vec<&[u8]> = raw_field
        .as_bytes()
        .chunks(width + 1)
        .map(|line| &line[..width])
        .collect();

    let mut start = (0, 0);
    let mut end = (0, 0);
    for (y, line) in field.iter().enumerate() {
        for (x, ch) in line.iter().enumerate() {
            match ch {
                b'S' => start = (x, y),
                b'E' => end = (x, y),
                _ => continue,
            }
        }
    }

    println!("{:?} {:?}", start, end);
    println!("{:?}", field);

    Ok(())
}
