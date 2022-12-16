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
    location_self: ValveId,
    location_elephant: ValveId,
    open_valves: u64,
    flow_rate: u32,
}

impl State {
    pub fn new() -> State {
        State {
            location_self: ValveId::from_str("AA"),
            location_elephant: ValveId::from_str("AA"),
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

    let n_useful_valves = valves.values().filter(|v| v.flow_rate > 0).count() as u32;

    // The states contain the state of the world as the key, and the amount of
    // pressure released so far as value.
    let mut states = HashMap::new();
    states.insert(State::new(), 0);

    for minute in 0..26 {
        let mut new_states: HashMap<State, u32> = HashMap::with_capacity(states.len());

        // We can either move, or open a valve. Doing nothing is never more
        // useful than moving.
        for (state, pressure_released) in states.iter() {
            let new_pressure = pressure_released + state.flow_rate;
            let mut insert_candidate = |mut state: State| {
                // The human and the elephant play an interchangeable role. If
                // we already consider state X, then we don't need to consider
                // state X with human and elephant swapped. So put them in a
                // canonical order, to make the state space smaller.
                if state.location_elephant > state.location_self {
                    std::mem::swap(&mut state.location_elephant, &mut state.location_self);
                }
                let pressure = new_states.entry(state).or_insert(new_pressure);
                *pressure = new_pressure.max(*pressure);
            };

            // If all valves are already open, then the best we can do is wait,
            // no need to move about any more.
            if state.open_valves.count_ones() == n_useful_valves {
                insert_candidate(state.clone());
                continue;
            }

            // If the valve is not open yet, we can open it. Only try to open
            // valves that have a positive flow rate, others are pointless and
            // lead to state explosion.
            let valve_self = &valves[&state.location_self];
            if valve_self.flow_rate > 0 && !state.is_open(valve_self) {
                let mut new_state = state.clone();
                new_state.open_valve(valve_self);

                // The elephant can also open one.
                let valve_elephant = &valves[&state.location_elephant];
                if valve_elephant.flow_rate > 0 && !new_state.is_open(valve_elephant) {
                    let mut new_new_state = new_state.clone();
                    new_new_state.open_valve(valve_elephant);
                    insert_candidate(new_new_state);
                }

                // Alternatively, the elephant can move.
                for id in &valves[&state.location_elephant].tunnels_to {
                    let mut new_new_state = new_state.clone();
                    new_new_state.location_elephant = *id;
                    insert_candidate(new_new_state);
                }
            }

            // Alternatively, we can move to a different tunnel.
            for id in &valves[&state.location_self].tunnels_to {
                let mut new_state = state.clone();
                new_state.location_self = *id;

                // The elephant can open a valve.
                let valve_elephant = &valves[&state.location_elephant];
                if valve_elephant.flow_rate > 0 && !new_state.is_open(valve_elephant) {
                    let mut new_new_state = new_state.clone();
                    new_new_state.open_valve(valve_elephant);
                    insert_candidate(new_new_state);
                }

                // Alternatively, the elephant can move.
                for id in &valves[&state.location_elephant].tunnels_to {
                    let mut new_new_state = new_state.clone();
                    new_new_state.location_elephant = *id;
                    insert_candidate(new_new_state);
                }
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
