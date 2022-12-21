use std::collections::HashSet;
use std::str::FromStr;
use std::{fs::File, io::Read};

#[derive(Debug, Hash, Eq, PartialEq)]
struct Droplet {
    x: isize,
    y: isize,
    z: isize,
}

impl Droplet {
    fn parse(input: &str) -> Droplet {
        let parts = input.split(",").collect::<Vec<_>>();
        Droplet {
            x: isize::from_str(parts[0]).unwrap(),
            y: isize::from_str(parts[1]).unwrap(),
            z: isize::from_str(parts[2]).unwrap(),
        }
    }

    fn faces(&self) -> Vec<Droplet> {
        vec![
            Droplet {
                x: self.x - 1,
                y: self.y,
                z: self.z,
            },
            Droplet {
                x: self.x + 1,
                y: self.y,
                z: self.z,
            },
            Droplet {
                x: self.x,
                y: self.y - 1,
                z: self.z,
            },
            Droplet {
                x: self.x,
                y: self.y + 1,
                z: self.z,
            },
            Droplet {
                x: self.x,
                y: self.y,
                z: self.z - 1,
            },
            Droplet {
                x: self.x,
                y: self.y,
                z: self.z + 1,
            },
        ]
    }
}

fn part1(contents: &mut String) {
    let droplets = HashSet::<Droplet>::from_iter(contents.lines().map(Droplet::parse));

    let mut uncovered = 0;
    for droplet in droplets.iter() {
        for face in droplet.faces() {
            if !droplets.contains(&face) {
                uncovered += 1;
            }
        }
    }

    println!("Total surface area: {}", uncovered);
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
