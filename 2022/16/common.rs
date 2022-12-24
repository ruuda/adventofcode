use std::collections::HashMap;
use std::env;
use std::fmt;
use std::fs;
use std::hash::Hash;
use std::str::FromStr;

/// Valve id, identified by two letters, AA through ZZ.
/// We take the value of the ASCII bytes, little endian.
#[derive(Copy, Clone, Eq, Ord, Hash, PartialEq, PartialOrd)]
pub struct ValveId(u16);

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
pub struct Valve {
    pub flow_rate: u32,
    pub tunnels_to: Vec<ValveId>,
    pub mask: u64,
}

pub fn load_input() -> HashMap<ValveId, Valve> {
    let mut output = HashMap::new();
    let input = fs::read_to_string(
        env::args()
            .skip(1)
            .next()
            .expect("Expected input fname as argument."),
    )
    .expect("Failed to read input.");

    for (i, line) in input.lines().enumerate() {
        let mask = 1_u64 << i;
        let parts = line.split(&[' ', '=', ';', ',']).filter(|s| !s.is_empty());
        // Example line:
        // Valve BB has flow rate=13; tunnels lead to valves CC, AA
        let mut parts = parts.skip(1);
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
            mask,
        };
        output.insert(id, valve);
    }

    output
}
