use std::cmp::Ordering;
use std::str::FromStr;
use std::{fs::File, io::Read};

#[derive(Debug, PartialEq, Eq, Clone)]
struct Packet {
    items: Vec<Item>,
}

impl Packet {
    fn from_single(single: usize) -> Packet {
        Packet {
            items: vec![Item::Number(single)],
        }
    }
}

impl PartialOrd for Packet {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let min_size = self.items.len().min(other.items.len());

        for ind in 0..min_size {
            match &self.items[ind].partial_cmp(&other.items[ind]) {
                None => panic!("Could not compare items"),
                Some(item_order) => match item_order {
                    Ordering::Equal => (),
                    order => return Some(*order),
                },
            }
        }

        if self.items.len() == other.items.len() {
            return Some(Ordering::Equal);
        }

        if min_size == self.items.len() {
            return Some(Ordering::Less);
        } else {
            return Some(Ordering::Greater);
        }
    }
}

impl Ord for Packet {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum Item {
    List(Box<Packet>),
    Number(usize),
}

impl PartialOrd for Item {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        //println!("Compare {:?} vs {:?})", self, other);
        match self {
            Item::Number(n) => match other {
                Item::Number(o) => Some(n.cmp(o)),
                Item::List(l) => Packet::from_single(*n).partial_cmp(l),
            },
            Item::List(l) => match other {
                Item::Number(n) => (**l).partial_cmp(&Packet::from_single(*n)),
                Item::List(o) => l.partial_cmp(o),
            },
        }
    }
}

fn parse_item(input: &str) -> Item {
    let n = usize::from_str(input);
    if let Ok(num) = n {
        return Item::Number(num);
    }

    Item::List(Box::new(Packet::parse(input)))
}

impl Packet {
    fn parse(input: &str) -> Packet {
        let mut packet = Packet { items: Vec::new() };

        let input = input.trim();

        if input.len() < 3 {
            return packet;
        }

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

        packet.items.push(parse_item(&input[last..input.len() - 1]));

        packet
    }
}

fn part1(contents: &mut String) {
    let pairs = contents.split("\n\n");

    let packets = pairs
        .map(|pair| pair.split_once("\n").unwrap())
        .map(|(first, second)| (Packet::parse(first), Packet::parse(second)))
        .collect::<Vec<_>>();

    let sum_indices: usize = packets
        .iter()
        .enumerate()
        .map(|(index, (left, right))| if left < right { index + 1 } else { 0 })
        .sum();

    println!("Sum of indices was {}", sum_indices);
}

fn part2(contents: &mut String) {
    let pairs = contents.split("\n\n");

    let mut packets = pairs
        .map(|pair| pair.split_once("\n").unwrap())
        .map(|(first, second)| vec![Packet::parse(first), Packet::parse(second)])
        .flatten()
        .collect::<Vec<_>>();

    let first_marker = Packet {
        items: vec![Item::List(Box::new(Packet::from_single(2)))],
    };
    let second_marker = Packet {
        items: vec![Item::List(Box::new(Packet::from_single(6)))],
    };

    packets.push(first_marker.clone());
    packets.push(second_marker.clone());

    packets.sort();

    let first_ind = packets
        .iter()
        .position(|pack| pack == &first_marker)
        .unwrap();

    let second_ind = packets
        .iter()
        .position(|pack| pack == &second_marker)
        .unwrap();

    println!(
        "Markers were found at {} and {}, decoder key is: {}",
        first_ind + 1,
        second_ind + 1,
        (first_ind + 1) * (second_ind + 1)
    );
}

fn main() -> std::io::Result<()> {
    let mut f = File::open("input")?;
    let mut contents = String::new();
    f.read_to_string(&mut contents)?;

    part1(&mut contents);
    part2(&mut contents);

    Ok(())
}
