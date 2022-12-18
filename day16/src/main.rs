mod cost;
mod tunnels;

use std::collections::HashMap;
use std::str::FromStr;
use std::{fs::File, io::Read};
use tunnels::Tunnels;

#[derive(Debug, Eq, Hash, PartialEq, Clone)]
pub struct Valve {
    name: String,
    flow_rate: usize,
    open: bool,
    tunnels: Vec<String>,
}

pub static TIME_LEFT: usize = 30;

fn part1(contents: &mut String) {
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
                name: valve_name.to_string(),
                flow_rate: usize::from_str(valve_rate).unwrap(),
                open: false,
                tunnels,
            },
        );
    }

    let tunnels = Tunnels { tunnels: valves };

    println!(
        "Maximum pressure: {}",
        tunnels.max_pressure("AA".to_string())
    );
}

//fn part2(contents: &mut String) {}

fn main() -> std::io::Result<()> {
    let mut f = File::open("input")?;
    let mut contents = String::new();
    f.read_to_string(&mut contents)?;

    part1(&mut contents);
    //part2(&mut contents);

    Ok(())
}
