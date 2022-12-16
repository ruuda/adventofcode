use std::collections::HashMap;
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
    mask: u64,
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

#[derive(Clone, Hash, Eq, PartialEq)]
struct State {
    location: ValveId,
    open_valves: u64,
    flow_rate: u32,
}

impl State {
    pub fn new() -> State {
        State {
            location: ValveId::from_str("AA"),
            open_valves: 0,
            flow_rate: 0,
        }
    }

    pub fn is_open(&self, valve: &Valve) -> bool {
        self.open_valves & valve.mask > 0
    }

    pub fn open_valve(&mut self, valve: &Valve) {
        self.open_valves |= valve.mask;
        self.flow_rate += valve.flow_rate;
    }
}

fn main() {
    let valves = load_input();

    // The states contain the state of the world as the key, and the amount of
    // pressure released so far as value.
    let mut states = HashMap::new();
    states.insert(State::new(), 0);

    for minute in 0..30 {
        let mut new_states: HashMap<State, u32> = HashMap::with_capacity(states.len());

        let mut insert_candidate = |state, new_pressure| {
            let pressure = new_states.entry(state).or_insert(new_pressure);
            *pressure = new_pressure.max(*pressure);
        };

        // We can either move, or open a valve. Doing nothing is never more
        // useful than moving.
        for (state, pressure_released) in states.iter() {
            // If the valve is not open yet, we can open it. Only try to open
            // valves that have a positive flow rate, others are pointless and
            // lead to state explosion.
            let valve = &valves[&state.location];
            if valve.flow_rate > 0 && !state.is_open(valve) {
                let mut new_state = state.clone();
                new_state.open_valve(valve);
                let new_pressure = pressure_released + state.flow_rate;
                insert_candidate(new_state, new_pressure);
            }

            // Alternatively, we can move to a different tunnel.
            for id in &valves[&state.location].tunnels_to {
                let mut new_state = state.clone();
                new_state.location = *id;
                let new_pressure = pressure_released + state.flow_rate;
                insert_candidate(new_state, new_pressure);
            }
        }
        println!(
            "minute={} states={} new_states={}",
            minute,
            states.len(),
            new_states.len()
        );
        states = new_states;
    }

    let max_flow = states.iter().max_by_key(|kv| kv.1).unwrap();
    println!("Pressure released: {}", max_flow.1);
}
