mod range;
mod sensor;

use std::str::FromStr;
use std::{fs::File, io::Read};

use sensor::Sensor;

use range::Range;

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct Point {
    pub x: isize,
    pub y: isize,
}

impl Point {
    pub fn manhattan_distance(&self, other: &Point) -> usize {
        self.x.abs_diff(other.x) + self.y.abs_diff(other.y)
    }
}

impl Point {
    fn parse(input: &str) -> Point {
        let (xpos, ypos) = input.split_once(", ").unwrap();
        let x = isize::from_str(&xpos[2..]).unwrap();
        let y = isize::from_str(&ypos[2..]).unwrap();
        Point { x, y }
    }
}

fn part1(contents: &mut String) {
    let mut sensors = contents.lines().map(Sensor::parse).collect::<Vec<Sensor>>();
    sensors.sort();

    let minx = sensors[0].position.x - sensors[0].distance_to_beacon() as isize;
    let maxx = sensors[sensors.len() - 1].position.x
        + sensors[sensors.len() - 1].distance_to_beacon() as isize;

    let counted_row = 2000000;

    let mut cannot_contain = 0;

    for ci in minx..maxx {
        let point = Point {
            x: ci,
            y: counted_row,
        };

        let mut can_contain = true;

        for sensor in &sensors {
            if sensor.point_is_within(&point) {
                can_contain = false;
            }
        }

        if can_contain {
        } else {
            cannot_contain += 1;
        }
    }

    println!("Points that cannot contain: {}", cannot_contain);
}

fn part2(contents: &mut String) {
    let sensors = contents.lines().map(Sensor::parse).collect::<Vec<Sensor>>();

    let mut found_beacon = Point { x: 0, y: 0 };

    const MIN_POS: isize = 0;
    const MAX_POS: isize = 4_000_000;
    //const MAX_POS: isize = 20;

    for row in MIN_POS..=MAX_POS {
        let mut row_range = Range::new();
        for sensor in &sensors {
            row_range.add_range(&sensor.range_at_row(row));
        }

        if let Some(found) = row_range.first_nonoverlap() {
            if found >= MIN_POS && found <= MAX_POS {
                found_beacon = Point { x: found, y: row };
                break;
            }
        }
    }

    println!(
        "Found one point: {:?}, frequency: {}",
        found_beacon,
        found_beacon.x * 4_000_000 + found_beacon.y
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
