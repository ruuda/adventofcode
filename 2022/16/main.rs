use std::collections::HashMap;
use std::env;
use std::fmt;
use std::fs;
use std::str::FromStr;

// Valve id, identified by two letters, AA through ZZ.
// We take the value of the ASCII bytes, little endian.
#[derive(Eq, Ord, Hash, PartialEq, PartialOrd)]
struct ValveId(u16);

impl ValveId {
    pub fn from_str(id: &str) -> ValveId {
        let bytes = id.as_bytes();
        let int = u16::from_le_bytes([bytes[0], bytes[1]]);
        ValveId(int)
    }
}

impl fmt::Debug for ValveId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

impl fmt::Display for ValveId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use std::str;
        let bytes = self.0.to_le_bytes();
        let as_str = str::from_utf8(&bytes).unwrap();
        as_str.fmt(f)
    }
}

#[derive(Debug)]
struct Valve {
    flow_rate: u32,
    tunnels_to: Vec<ValveId>,
}

fn load_input() -> HashMap<ValveId, Valve> {
    let mut output = HashMap::new();
    let input = fs::read_to_string(
        env::args()
            .skip(1)
            .next()
            .expect("Expected input fname as argument."),
    )
    .expect("Failed to read input.");
    for line in input.lines() {
        let parts = line.split(&[' ', '=', ';', ',']).filter(|s| !s.is_empty());
        // Example line:
        // Valve BB has flow rate=13; tunnels lead to valves CC, AA
        let mut parts = parts.skip(1);
        //parts.next().expect("syntax error");
        let id_str = parts.next().expect("syntax error");
        let mut parts = parts.skip(3);
        let flow_rate_str = parts.next().expect("syntax error");
        let parts = parts.skip(4);

        let id = ValveId::from_str(id_str);
        let flow_rate = u32::from_str(flow_rate_str).expect("syntax error");
        let tunnels_to = parts.map(|s| ValveId::from_str(s)).collect();
        let valve = Valve {
            flow_rate,
            tunnels_to,
        };
        output.insert(id, valve);
    }

    output
}

fn main() {
    let valves = load_input();
    for (id, v) in &valves {
        println!("{} -> {} {:?}", id, v.flow_rate, v.tunnels_to);
    }
}
