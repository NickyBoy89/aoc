use std::{
    collections::{BinaryHeap, HashMap},
    hash::Hash,
};

use crate::cost::Cost;
use crate::Valve;
use crate::TIME_LEFT;

#[derive(Hash, Clone)]
struct Tunnel {
    name: String,
    priority: usize,
}

impl Tunnel {
    fn from_str(input: &String) -> Tunnel {
        Tunnel {
            name: input.clone(),
            priority: 0,
        }
    }
}

impl PartialOrd for Tunnel {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Tunnel {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.priority.cmp(&other.priority)
    }
}

impl PartialEq for Tunnel {
    fn eq(&self, other: &Self) -> bool {
        self.priority == other.priority
    }
}

impl Eq for Tunnel {}

pub struct Tunnels {
    pub tunnels: HashMap<String, Valve>,
}

impl Tunnels {
    pub fn max_pressure(&self, start: String) -> usize {
        let source = Tunnel::from_str(&start);

        let mut frontier = BinaryHeap::from([source]);

        let mut came_from = HashMap::new();
        let mut cost_so_far = HashMap::new();

        came_from.insert(start.to_string(), Tunnel::from_str(&"".to_string()));
        cost_so_far.insert(start.to_string(), Cost::new(0));

        while frontier.len() > 0 {
            let current = frontier.pop().unwrap();
            let current_cost = cost_so_far[current.name.as_str().clone()];

            if current_cost.time_remaining == 0 {
                println!(
                    "Total released in path was {}",
                    current_cost.pressure_released
                );
            }

            // Look at all the valves that are accessible from the current valve
            for next in self.neighbors_within_time(&current.name, current_cost.time_remaining) {
                let next = Tunnel::from_str(&next);

                let time_cost = match self.travel_time(&current.name, &next.name) {
                    Some(cost) => cost,
                    None => continue,
                };
                let pressure_released =
                    self.flow_rate_of(&current.name) * (current_cost.time_remaining - 1);

                // Current path plus the cost from current to next
                let new_cost = cost_so_far[&current.name]
                    + Cost {
                        pressure_released,
                        time_remaining: TIME_LEFT - time_cost,
                    };

                if !cost_so_far.contains_key(&next.name) || new_cost > cost_so_far[&next.name] {
                    cost_so_far.insert(next.name.clone(), new_cost);
                    let item = Tunnel {
                        name: next.name.clone(),
                        priority: new_cost.pressure_released,
                    };
                    frontier.push(item);
                    came_from.insert(next.name, current.clone());
                }
            }
        }

        0
    }

    /// travel_time returns the time cost from one valve to another valve
    fn travel_time(&self, from: &String, to: &String) -> Option<usize> {
        let mut frontier = BinaryHeap::from([Tunnel::from_str(from)]);

        let mut came_from = HashMap::<Tunnel, Tunnel>::new();
        let mut cost_so_far = HashMap::<Tunnel, usize>::new();

        came_from.insert(Tunnel::from_str(&from), Tunnel::from_str(&"".to_string()));
        cost_so_far.insert(Tunnel::from_str(&from), 0);

        while frontier.len() > 0 {
            let current = frontier.pop().unwrap();
            if current.name == *to {
                return Some(current.priority);
            }

            for next in self.neighbors_of(&current.name) {
                let next = Tunnel::from_str(&next);
                let new_cost = *cost_so_far.get(&current).unwrap_or(&0) + 1;
                if !cost_so_far.contains_key(&next) || new_cost < cost_so_far[&next] {
                    cost_so_far.insert(next.clone(), new_cost);
                    let item = Tunnel {
                        name: next.name.clone(),
                        priority: new_cost,
                    };
                    frontier.push(item);
                    came_from.insert(next, current.clone());
                }
            }
        }

        None
    }

    /// neighbors_of returns the names of the valves that a single tunnnel connects to
    fn neighbors_of(&self, name: &String) -> Vec<String> {
        self.tunnels[name].tunnels.clone()
    }

    fn neighbors_within_time(&self, name: &String, time_budget: usize) -> Vec<String> {
        self.tunnels[name]
            .tunnels
            .iter()
            .cloned()
            .filter(|tunnel_name| {
                let time_cost = self.travel_time(name, tunnel_name);
                time_cost.is_some() && time_cost.unwrap() <= time_budget
            })
            .collect()
    }

    fn flow_rate_of(&self, name: &String) -> usize {
        if self.tunnels[name].open {
            return 0;
        }
        self.tunnels[name].flow_rate
    }
}
