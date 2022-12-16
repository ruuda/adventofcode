use std::collections::HashMap;
use std::hash::Hash;

mod common;
use common::{load_input, Valve, ValveId};

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
