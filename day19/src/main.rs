use std::{
    collections::{BinaryHeap, HashSet},
    fs::File,
    hash::{Hash, Hasher},
    io::Read,
    str::FromStr,
};

#[derive(Debug)]
struct Cost {
    ore_cost: usize,
    clay_cost: usize,
    obsidian_cost: usize,
}

#[derive(Debug)]
struct Blueprint {
    ore_robot: Cost,
    clay_robot: Cost,
    obsidian_robot: Cost,
    geode_robot: Cost,
}

impl Blueprint {
    fn parse(input: &str) -> Blueprint {
        let words = input.split(' ').collect::<Vec<&str>>();
        Blueprint {
            ore_robot: Cost {
                ore_cost: usize::from_str(words[6]).unwrap(),
                clay_cost: 0,
                obsidian_cost: 0,
            },
            clay_robot: Cost {
                ore_cost: usize::from_str(words[12]).unwrap(),
                clay_cost: 0,
                obsidian_cost: 0,
            },
            obsidian_robot: Cost {
                ore_cost: usize::from_str(words[18]).unwrap(),
                clay_cost: usize::from_str(words[21]).unwrap(),
                obsidian_cost: 0,
            },
            geode_robot: Cost {
                ore_cost: usize::from_str(words[27]).unwrap(),
                clay_cost: 0,
                obsidian_cost: usize::from_str(words[30]).unwrap(),
            },
        }
    }

    fn highest_ore_cost(&self) -> usize {
        [
            self.ore_robot.ore_cost,
            self.clay_robot.ore_cost,
            self.obsidian_robot.ore_cost,
            self.geode_robot.ore_cost,
        ]
        .into_iter()
        .max()
        .unwrap()
    }
}

#[derive(Debug, Clone)]
struct Backpack {
    ore: usize,
    ore_robots: usize,
    clay: usize,
    clay_robots: usize,
    obsidian: usize,
    obsidian_robots: usize,
    geodes: usize,
    geode_robots: usize,
    time_left: usize,
}

impl PartialOrd for Backpack {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Backpack {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.time_left.cmp(&other.time_left)
    }
}

impl PartialEq for Backpack {
    fn eq(&self, other: &Self) -> bool {
        self.ore == other.ore
            && self.ore_robots == other.ore_robots
            && self.clay == other.clay
            && self.clay_robots == other.clay_robots
            && self.obsidian == other.obsidian
            && self.obsidian_robots == other.obsidian_robots
            && self.geodes == other.geodes
            && self.geode_robots == other.geode_robots
    }
}

impl Eq for Backpack {}

impl Hash for Backpack {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.ore.hash(state);
        self.ore_robots.hash(state);
        self.clay.hash(state);
        self.clay_robots.hash(state);
        self.obsidian.hash(state);
        self.obsidian_robots.hash(state);
        self.geodes.hash(state);
        self.geode_robots.hash(state);
    }
}

impl Backpack {
    fn initial() -> Backpack {
        Backpack {
            ore: 0,
            ore_robots: 1,
            clay: 0,
            clay_robots: 0,
            obsidian: 0,
            obsidian_robots: 0,
            geodes: 0,
            geode_robots: 0,
            time_left: 24,
        }
    }

    fn extended() -> Backpack {
        Backpack {
            ore: 0,
            ore_robots: 1,
            clay: 0,
            clay_robots: 0,
            obsidian: 0,
            obsidian_robots: 0,
            geodes: 0,
            geode_robots: 0,
            time_left: 32,
        }
    }

    fn update(&mut self) {
        self.ore += self.ore_robots;
        self.clay += self.clay_robots;
        self.obsidian += self.obsidian_robots;
        self.geodes += self.geode_robots;
    }
}

fn part1(contents: &mut str) {
    let blueprints = contents.lines().map(Blueprint::parse).collect::<Vec<_>>();

    let mut quality_levels = Vec::new();

    for (ind, selected) in blueprints.iter().enumerate() {
        let mut states = BinaryHeap::from([Backpack::initial()]);

        let most_expensive = [
            selected.ore_robot.ore_cost,
            selected.clay_robot.ore_cost,
            selected.obsidian_robot.ore_cost,
            selected.geode_robot.ore_cost,
        ]
        .into_iter()
        .max()
        .unwrap();

        let mut max_geodes = 0;

        let mut history = HashSet::new();

        while !states.is_empty() {
            let current_state = states.pop().unwrap();

            if history.contains(&current_state) {
                continue;
            }

            history.insert(current_state.clone());

            if current_state.ore_robots > most_expensive {
                continue;
            }

            // Ignore that state
            if current_state.time_left == 0 {
                if current_state.geodes > max_geodes {
                    max_geodes = current_state.geodes;
                }
                continue;
            }

            // Do nothing and wait for resources
            let mut new_state = current_state.clone();
            new_state.update();
            new_state.time_left -= 1;
            states.push(new_state);

            // Build an ore robot
            if current_state.ore >= selected.ore_robot.ore_cost {
                let mut new_state = current_state.clone();
                new_state.ore -= selected.ore_robot.ore_cost;
                new_state.update();
                new_state.ore_robots += 1;
                new_state.time_left -= 1;
                states.push(new_state);
            }

            // Build a clay robot
            if current_state.ore >= selected.clay_robot.ore_cost {
                let mut new_state = current_state.clone();
                new_state.ore -= selected.clay_robot.ore_cost;
                new_state.update();
                new_state.clay_robots += 1;
                new_state.time_left -= 1;
                states.push(new_state);
            }

            // Build an obsidian robot
            if current_state.ore >= selected.obsidian_robot.ore_cost
                && current_state.clay >= selected.obsidian_robot.clay_cost
            {
                let mut new_state = current_state.clone();
                new_state.ore -= selected.obsidian_robot.ore_cost;
                new_state.clay -= selected.obsidian_robot.clay_cost;
                new_state.update();
                new_state.obsidian_robots += 1;
                new_state.time_left -= 1;
                states.push(new_state);
            }

            // Build a geode robot
            if current_state.ore >= selected.geode_robot.ore_cost
                && current_state.obsidian >= selected.geode_robot.obsidian_cost
            {
                let mut new_state = current_state.clone();
                new_state.ore -= selected.geode_robot.ore_cost;
                new_state.obsidian -= selected.geode_robot.obsidian_cost;
                new_state.update();
                new_state.geode_robots += 1;
                new_state.time_left -= 1;
                states.push(new_state);
            }
        }

        println!("Blueprint {} has a max {} geodes opened", ind, max_geodes);

        quality_levels.push((ind + 1) * max_geodes);
    }

    println!(
        "Sum of qualities is: {}",
        quality_levels.iter().sum::<usize>()
    );
}

fn part2(contents: &mut str) {
    let mut blueprints = contents.lines().map(Blueprint::parse).collect::<Vec<_>>();
    blueprints.truncate(3);

    let mut quality_levels = Vec::new();

    for (ind, selected) in blueprints.iter().enumerate() {
        let mut states = BinaryHeap::from([Backpack::extended()]);

        let mut max_geodes = 0;

        let most_expensive = selected.highest_ore_cost();

        let mut history = HashSet::new();

        while !states.is_empty() {
            let mut current_state = states.pop().unwrap();

            let max_possible_ore = current_state.time_left * most_expensive;

            if current_state.ore > max_possible_ore {
                current_state.ore = max_possible_ore;
            }

            if history.contains(&current_state) {
                continue;
            }

            history.insert(current_state.clone());

            // Since we can only produce one robot each turn, discard options that
            // produce more resources that can be spent
            if current_state.ore_robots > most_expensive {
                continue;
            } else if current_state.clay_robots > selected.obsidian_robot.clay_cost {
                continue;
            } else if current_state.obsidian_robots > selected.geode_robot.obsidian_cost {
                continue;
            }

            // Ignore that state
            if current_state.time_left == 0 {
                //println!("Finished with max geodes: {}", max_geodes);
                if current_state.geodes > max_geodes {
                    max_geodes = current_state.geodes;
                }
                continue;
            }

            // Build a geode robot
            if current_state.ore >= selected.geode_robot.ore_cost
                && current_state.obsidian >= selected.geode_robot.obsidian_cost
            {
                let mut new_state = current_state.clone();
                new_state.ore -= selected.geode_robot.ore_cost;
                new_state.obsidian -= selected.geode_robot.obsidian_cost;
                new_state.update();
                new_state.geode_robots += 1;
                new_state.time_left -= 1;
                states.push(new_state);
                continue;
            }

            // Do nothing and wait for resources
            let mut new_state = current_state.clone();
            new_state.update();
            new_state.time_left -= 1;
            states.push(new_state);

            // Build an ore robot
            if current_state.ore >= selected.ore_robot.ore_cost {
                let mut new_state = current_state.clone();
                new_state.ore -= selected.ore_robot.ore_cost;
                new_state.update();
                new_state.ore_robots += 1;
                new_state.time_left -= 1;
                states.push(new_state);
            }

            // Build a clay robot
            if current_state.ore >= selected.clay_robot.ore_cost {
                let mut new_state = current_state.clone();
                new_state.ore -= selected.clay_robot.ore_cost;
                new_state.update();
                new_state.clay_robots += 1;
                new_state.time_left -= 1;
                states.push(new_state);
            }

            // Build an obsidian robot
            if current_state.ore >= selected.obsidian_robot.ore_cost
                && current_state.clay >= selected.obsidian_robot.clay_cost
            {
                let mut new_state = current_state.clone();
                new_state.ore -= selected.obsidian_robot.ore_cost;
                new_state.clay -= selected.obsidian_robot.clay_cost;
                new_state.update();
                new_state.obsidian_robots += 1;
                new_state.time_left -= 1;
                states.push(new_state);
            }
        }

        println!("Blueprint {} has a max {} geodes opened", ind, max_geodes);

        quality_levels.push(max_geodes);
    }

    let mut max = 1;
    for max_geodes in quality_levels {
        max *= max_geodes;
    }

    println!("Numbers multiplied together is: {}", max);
}

fn main() -> std::io::Result<()> {
    let mut f = File::open("input")?;
    let mut contents = String::new();
    f.read_to_string(&mut contents)?;

    part1(&mut contents);
    part2(&mut contents);

    Ok(())
}
