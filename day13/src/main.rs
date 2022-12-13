use std::str::FromStr;
use std::{fs::File, io::Read};

#[derive(Debug)]
struct Packet {
    items: Vec<usize>,
}

fn parse_item(input: &str) -> usize {
    let n = usize::from_str(input);
    match n {
        Ok(n) => n,
        Err(_) => 0,
    }
}

impl Packet {
    fn parse(input: &str) -> Packet {
        let mut packet = Packet { items: Vec::new() };

        let mut last = 1;
        let mut bracketdiff: isize = 0;
        for curr_ind in 1..input.len() {
            match input.as_bytes()[curr_ind] as char {
                '[' => bracketdiff -= 1,
                ']' => bracketdiff += 1,
                ',' => {
                    if bracketdiff == 0 {
                        packet.items.push(parse_item(&input[last..curr_ind]));
                        last = curr_ind + 1;
                    }
                }
                _ => (),
            }
        }

        packet
    }
}

fn part1(contents: &mut String) {
    let pairs = contents.split("\n\n");

    let packets = pairs
        .map(|pair| pair.split_once("\n").unwrap())
        .map(|(first, second)| (Packet::parse(first), Packet::parse(second)))
        .collect::<Vec<_>>();

    println!("{:?}", packets);
}

fn part2(contents: &mut String) {}

fn main() -> std::io::Result<()> {
    let mut f = File::open("input")?;
    let mut contents = String::new();
    f.read_to_string(&mut contents)?;

    part1(&mut contents);
    part2(&mut contents);

    Ok(())
}
