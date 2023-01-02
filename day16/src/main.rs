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
    fn update_pressure(&mut self, valves: &HashMap<String, Valve>) {
        for (valve, open) in self.valve_statuses.iter() {
            if *open {
                self.pressure_dispersed += valves[valve].flow_rate;
            }
        }
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

pub static TIME_LEFT: usize = 30;

fn part1(valves: &HashMap<String, Valve>) {
    let mut states = BinaryHeap::from([Cost {
        current_valve: "AA".to_string(),
        pressure_dispersed: 0,
        time_spent: 0,
        valve_statuses: valves
            .iter()
            .map(|(name, data)| (name.clone(), data.open))
            .collect::<Vec<_>>(),
    }]);

    let mut history = HashSet::new();

    let mut found_path = false;
    let mut max_dispersed = 0;

    while !states.is_empty() {
        let current = states.pop().unwrap();

        //println!("Current state: {:?}", current);

        if current.time_spent == 30 {
            if current.pressure_dispersed > max_dispersed {
                if !found_path {
                    found_path = true;
                }
                max_dispersed = current.pressure_dispersed;
                println!("Max is now {}", max_dispersed);
            }
            continue;
        }

        if !history.insert(current.clone()) {
            continue;
        }

        // Add one state for opening the current valve
        let mut newly_opened = current.clone();
        newly_opened.open_valve(&current.current_valve);
        newly_opened.update_pressure(valves);
        newly_opened.time_spent += 1;
        states.push(newly_opened);

        for (valve, _) in current.valve_statuses.iter() {
            let mut other_valve = current.clone();
            other_valve.current_valve = valve.clone();
            other_valve.update_pressure(valves);
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
