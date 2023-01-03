#![feature(binary_heap_retain)]

use std::collections::{BinaryHeap, HashMap, HashSet};
use std::str::FromStr;
use std::{fs::File, io::Read};

#[derive(Debug, Eq, Hash, PartialEq, Clone)]
pub struct Valve {
    flow_rate: usize,
    open: bool,
    leads_to: Vec<String>,
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
struct Cost {
    current_valve: String,
    pressure_dispersed: usize,
    time_spent: usize,
    valve_statuses: Vec<(String, bool)>,
}

impl Cost {
    fn is_open(&self, name: &String) -> bool {
        for (valve_name, open) in self.valve_statuses.iter() {
            if *valve_name == *name {
                return *open;
            }
        }
        panic!("Did not find valve {}", name);
    }

    /// open_valve returns a bool that says if the value in the cost was modified
    fn open_valve(&mut self, name: &String) -> bool {
        for (valve, open) in self.valve_statuses.iter_mut() {
            if *valve == *name {
                let modified = !*open;
                *open = true;
                return modified;
            }
        }
        panic!("Could not find target valve: {}", name);
    }

    fn has_open_zero(&self, costs: &Vec<(usize, String)>) -> bool {
        for (cost, valve_name) in costs {
            if *cost == 0 && self.is_open(valve_name) {
                return true;
            }
        }
        false
    }

    fn maximum_potential(&self, costs: &Vec<(usize, String)>) -> usize {
        let mut potential = 0;
        let mut current_time = self.time_spent;
        let mut costs = costs.clone();
        costs.reverse();
        for (flow_rate, name) in costs {
            if !self.is_open(&name) {
                if current_time < MAX_TIME {
                    potential += (MAX_TIME - current_time) * flow_rate;
                    current_time += 1;
                }
            }
        }

        //println!("Potential for {:?} is {}", self, potential);

        potential
    }
}

impl PartialOrd for Cost {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(&other))
    }
}

impl Ord for Cost {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.pressure_dispersed.cmp(&other.pressure_dispersed)
    }
}

static MAX_TIME: usize = 30;

fn part1(valves: &HashMap<String, Valve>) {
    let mut costs = valves
        .iter()
        .map(|(name, data)| (data.flow_rate, name.clone()))
        .collect::<Vec<_>>();
    costs.sort();

    let mut states = BinaryHeap::from([Cost {
        current_valve: "AA".to_string(),
        pressure_dispersed: 0,
        time_spent: 0,
        valve_statuses: valves
            .iter()
            .map(|(name, data)| (name.clone(), data.open))
            .collect::<Vec<_>>(),
    }]);

    let mut history = HashSet::<Cost>::new();

    let mut found_max = false;
    let mut max_dispersed = 0;

    let mut dispersed = usize::MAX;

    while !states.is_empty() {
        let current = states.pop().unwrap();

        //println!("Current state: {:?}", current);
        dispersed -= 1;
        if dispersed == 0 {
            panic!("Max iter");
        }

        if found_max
            && current.maximum_potential(&costs) + current.pressure_dispersed < max_dispersed
        {
            continue;
        }

        if current.has_open_zero(&costs) {
            continue;
        }

        if current.time_spent == MAX_TIME {
            if current.pressure_dispersed > max_dispersed {
                if !found_max {
                    found_max = true;
                }
                max_dispersed = current.pressure_dispersed;
                history.retain(|state| {
                    state.maximum_potential(&costs) + state.pressure_dispersed >= max_dispersed
                });
                states.retain(|state| {
                    state.maximum_potential(&costs) + state.pressure_dispersed >= max_dispersed
                });
                println!("{:?}", current);
                println!("Max is now {}", max_dispersed);
            }
            continue;
        }

        if !history.insert(current.clone()) {
            continue;
        }

        // Add one state for opening the current valve
        let mut newly_opened = current.clone();
        newly_opened.time_spent += 1;
        if newly_opened.open_valve(&current.current_valve) {
            newly_opened.pressure_dispersed +=
                valves[&current.current_valve].flow_rate * (MAX_TIME - newly_opened.time_spent);
        }
        states.push(newly_opened);

        for valve in valves[&current.current_valve].leads_to.iter() {
            let mut other_valve = current.clone();
            other_valve.current_valve = valve.clone();
            other_valve.time_spent += 1;
            states.push(other_valve);
        }
    }

    println!("Found a path that dispersed {} pressure", max_dispersed);
}

//fn part2(contents: &mut String) {}

fn main() -> std::io::Result<()> {
    let mut f = File::open("input")?;
    let mut contents = String::new();
    f.read_to_string(&mut contents)?;

    let mut valves = HashMap::new();

    for line in contents.lines() {
        let (valve, tunnels) = line.split_once("; ").unwrap();

        let valve_words = valve.split(" ").collect::<Vec<_>>();
        let valve_name = valve_words[1];
        let valve_rate = &valve_words[4]["rate=".len()..];

        let tunnels = &tunnels.split(" ").collect::<Vec<_>>()[4..];
        let tunnels: Vec<String> = tunnels
            .iter()
            .map(|name| {
                match name.strip_suffix(",") {
                    Some(new_name) => new_name,
                    None => name,
                }
                .to_string()
            })
            .collect();

        valves.insert(
            valve_name.to_string(),
            Valve {
                flow_rate: usize::from_str(valve_rate).unwrap(),
                open: false,
                leads_to: tunnels,
            },
        );
    }

    part1(&valves);
    //part2(&mut contents);

    Ok(())
}
