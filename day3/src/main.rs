#![feature(array_chunks)]
//#![feature(iter_array_chunks)]
use std::{fs::File, io::Read};

fn priority(c: char) -> u32 {
    if c.is_ascii_lowercase() {
        return (c as u8 - b'a') as u32 + 1;
    }
    return (c as u8 - b'A') as u32 + 27;
}

fn part1(contents: &mut String) {
    let mut total = 0;

    for line in contents.split("\n") {
        let cutoff = line.len() / 2;

        let mut matches: Vec<char> = vec![];

        for c in line[..cutoff].chars() {
            if line[cutoff..].rfind(c) != None {
                matches.push(c);
            }
        }

        matches.sort();
        matches.dedup();

        for m in matches {
            println!("Priority of {} was {}", m, priority(m));
            total += priority(m);
        }
    }

    println!("Total is {}", total);
}

fn part2(contents: &mut String) {
    let mut total = 0;

    let lines: Vec<&str> = contents.lines().collect();

    for index in 0..lines.len() - 1 {
        if index % 3 == 0 {
            println!("{}", index);
            let mut badge: Option<char> = None;

            for c in lines[index].chars() {
                if lines[index + 1].rfind(c) != None && lines[index + 2].rfind(c) != None {
                    badge = Some(c);
                }
            }

            println!("Priority of {:?} was {}", badge, priority(badge.unwrap()));
            total += priority(badge.unwrap());
        }
    }

    println!("Total is {}", total);
}

fn main() -> std::io::Result<()> {
    let mut f = File::open("input")?;
    let mut contents = String::new();
    f.read_to_string(&mut contents)?;

    //part1(&mut contents);
    part2(&mut contents);

    println!("Hello, world!");
    Ok(())
}
