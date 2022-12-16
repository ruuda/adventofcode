use std::collections::{HashSet, HashMap, BTreeMap};
use std::env;
use std::fmt;
use std::fs;
use std::hash::Hash;
use std::str::FromStr;

// Valve id, identified by two letters, AA through ZZ.
// We take the value of the ASCII bytes, little endian.
#[derive(Copy, Clone, Eq, Ord, Hash, PartialEq, PartialOrd)]
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

#[derive(Clone, Hash, Eq, PartialEq)]
struct State {
    location: ValveId,
    open_valves: BTreeMap<ValveId, u32>,
    pressure_released: u32,
}

impl State {
    pub fn new() -> State {
        State {
            location: ValveId::from_str("AA"),
            open_valves: BTreeMap::new(),
            pressure_released: 0,
        }
    }

    pub fn tick_minute(&self) -> State {
        let mut state = self.clone();
        state.pressure_released += self.open_valves.values().sum::<u32>();
        state
    }
}

fn main() {
    let valves = load_input();

    let mut states = HashSet::new();
    states.insert(State::new());

    for minute in 0..30 {
        let mut new_states = HashSet::with_capacity(states.len());

        // We take all of the states to the next minute, apply the flow of that.
        for state in states.iter() {
            new_states.insert(state.tick_minute());
        }

        // Then for those states, we can also either move, or open a valve.
        for state in states.iter() {
            // If the valve is not open yet, we can open it.
            if !state.open_valves.contains_key(&state.location) {
                let mut new_state = state.tick_minute();
                new_state.open_valves.insert(state.location, valves[&state.location].flow_rate);
                new_states.insert(new_state);
            }

            // Alternatively, we can move to a different tunnel.
            for id in &valves[&state.location].tunnels_to {
                let mut new_state = state.tick_minute();
                new_state.location = *id;
                new_states.insert(new_state);
            }
        }
        println!("minute={} states={} new_states={}", minute, states.len(), new_states.len());

        states.extend(new_states.into_iter());
    }

    let max_flow = states.iter().max_by_key(|s| s.pressure_released).unwrap();
    println!("Pressure released: {}", max_flow.pressure_released);
}
