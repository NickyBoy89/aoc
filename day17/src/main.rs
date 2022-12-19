mod rocks;

use std::collections::HashSet;
use std::time::Instant;
use std::{fs::File, io::Read};

use rocks::*;

#[derive(Debug)]
enum Direction {
    Left,
    Right,
}

impl Direction {
    fn parse(input: char) -> Option<Direction> {
        match input {
            '<' => Some(Direction::Left),
            '>' => Some(Direction::Right),
            _ => None,
        }
    }
}

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
pub struct Point {
    pub x: isize,
    pub y: usize,
}

#[derive(Debug, Clone)]
pub struct Rock {
    pub points: Vec<Point>,
    stopped: bool,

    xmin: isize,
    xmax: isize,
    ymin: usize,
    ymax: usize,
}

impl Rock {
    pub fn new(points: Vec<Point>) -> Rock {
        let start = Instant::now();
        let xmin = points.iter().map(|point| point.x).min().unwrap();
        let xmax = points.iter().map(|point| point.x).max().unwrap();
        let ymin = points.iter().map(|point| point.y).min().unwrap();
        let ymax = points.iter().map(|point| point.y).max().unwrap();

        println!("Creating a point took {:?}", start.elapsed());

        Rock {
            points,
            stopped: false,
            xmin,
            xmax,
            ymin,
            ymax,
        }
    }

    fn shift_x(&self, amount: isize) -> Rock {
        let mut new_rock = self.clone();
        for point in new_rock.points.iter_mut() {
            point.x += amount;
        }
        new_rock.xmin += amount;
        new_rock.xmax += amount;

        new_rock
    }

    fn shift_y(&self, amount: isize) -> Rock {
        let mut new_rock = self.clone();
        for point in new_rock.points.iter_mut() {
            point.y = (point.y as isize + amount) as usize;
        }
        new_rock.ymin = (new_rock.ymin as isize + amount) as usize;
        new_rock.ymax = (new_rock.ymax as isize + amount) as usize;

        new_rock
    }

    fn overlaps(&self, other: &Rock) -> bool {
        !HashSet::<&Point>::from_iter(self.points.iter())
            .is_disjoint(&HashSet::from_iter(other.points.iter()))
    }
}

impl PartialEq for Rock {
    fn eq(&self, other: &Self) -> bool {
        self.points == other.points
    }
}

impl Eq for Rock {}

impl PartialOrd for Rock {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Rock {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        other.ymax.cmp(&self.ymax)
    }
}

struct Chamber {
    rocks: Vec<Rock>,
    highest: usize,
}

impl Chamber {
    fn new() -> Chamber {
        Chamber {
            rocks: Vec::new(),
            highest: 0,
        }
    }

    fn add_rock(&mut self, rock: Rock) -> usize {
        let start = Instant::now();
        self.highest = rock.ymax + 1;
        self.rocks.push(rock.clone());
        self.rocks.sort();
        let rock_position = self.rocks.iter().position(|r| *r == rock).unwrap();

        println!("Adding a rock took {:?}", start.elapsed());
        rock_position
    }

    fn recalculate_highest(&mut self) {
        self.highest = self.rocks.iter().map(|rock| rock.ymax).max().unwrap() + 1
    }

    fn display(&self) {
        let points = HashSet::<Point>::from_iter(
            self.rocks.iter().map(|rock| rock.points.clone()).flatten(),
        );

        for ri in (0..=self.highest).rev() {
            for ci in 0..7 {
                if points.contains(&Point { x: ci, y: ri }) {
                    print!("#");
                } else {
                    print!(".");
                }
            }
            println!();
        }
    }

    fn rock_stopped(&self, index: usize) -> bool {
        self.rocks[index].stopped
    }
}

fn part1(contents: &mut String) {
    let directions = contents
        .chars()
        .map(Direction::parse)
        .filter(|d| d.is_some())
        .map(|d| d.unwrap())
        .collect::<Vec<_>>();
    let mut direction_index = 0;
    // seven units wide
    // gets pushed first, falls second
    // left edge is two units away from the left wall
    // bottom is three units above the highest rock, or floor

    let rotation = Vec::from([line, plus, ell, vline, square]);

    let mut chamber = Chamber::new();

    static NUM_ROCKS: usize = 2022;

    for rock_count in 0..NUM_ROCKS {
        let rock_ind = chamber.add_rock(rotation[rock_count % rotation.len()](chamber.highest + 3));

        println!("Rock {} begins falling", rock_count + 1);

        while !chamber.rock_stopped(rock_ind) {
            let other_rocks = chamber
                .rocks
                .iter()
                .cloned()
                .enumerate()
                .filter(|(ind, _)| *ind != rock_ind)
                .map(|(_, rock)| rock)
                .collect::<Vec<Rock>>();

            let dir = &directions[direction_index % directions.len()];
            direction_index += 1;

            let shifted = match dir {
                Direction::Left => chamber.rocks[rock_ind].shift_x(-1),
                Direction::Right => chamber.rocks[rock_ind].shift_x(1),
            };

            if shifted.xmin > 0 && shifted.xmax < 7 {
                let overlaps = other_rocks
                    .iter()
                    .find(|rock| shifted.overlaps(rock))
                    .is_some();

                if !overlaps {
                    chamber.rocks[rock_ind] = shifted;
                }
            }

            //println!("Moving in direction {:?}", dir);
            //chamber.display();

            if chamber.rocks[rock_ind].ymin == 0 {
                chamber.rocks[rock_ind].stopped = true;
                chamber.recalculate_highest();
            } else {
                let gravity = chamber.rocks[rock_ind].shift_y(-1);

                let overlaps = other_rocks
                    .iter()
                    .find(|rock| gravity.overlaps(rock))
                    .is_some();

                if overlaps {
                    chamber.rocks[rock_ind].stopped = true;
                    chamber.recalculate_highest();
                } else {
                    chamber.rocks[rock_ind] = gravity;
                }
            }

            //println!("After gravity:");
            //chamber.display();
        }

        println!("The {}th rock stopped falling", rock_count + 1);
    }

    println!("Maximum height: {}", chamber.highest);
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
