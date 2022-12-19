mod rocks;

use std::collections::HashSet;
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
}

impl Rock {
    fn max_x(&self) -> isize {
        self.points.iter().map(|point| point.x).max().unwrap()
    }

    fn min_x(&self) -> isize {
        self.points.iter().map(|point| point.x).min().unwrap()
    }

    fn max_y(&self) -> usize {
        self.points.iter().map(|point| point.y).max().unwrap()
    }

    fn min_y(&self) -> usize {
        self.points.iter().map(|point| point.y).min().unwrap()
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
        other.max_y().cmp(&self.max_y())
    }
}

struct Chamber {
    rocks: Vec<Rock>,
}

impl Chamber {
    fn new() -> Chamber {
        Chamber { rocks: Vec::new() }
    }

    fn add_rock(&mut self, rock: Rock) -> usize {
        println!("Highest is {}", rock.max_y());
        self.rocks.push(rock.clone());
        self.rocks.sort();
        self.rocks.iter().position(|r| *r == rock).unwrap()
    }

    fn max_height(&self) -> usize {
        if self.rocks.len() == 0 {
            return 0;
        }
        self.rocks.iter().map(|rock| rock.max_y()).max().unwrap() + 1
    }

    fn display(&self) {
        let points = HashSet::<Point>::from_iter(
            self.rocks.iter().map(|rock| rock.points.clone()).flatten(),
        );

        for ri in (0..=self.max_height()).rev() {
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

    fn shift_rock_x(&self, index: usize, amount: isize) -> Rock {
        Rock {
            points: self.rocks[index]
                .points
                .iter()
                .cloned()
                .map(|mut point| {
                    point.x += amount;

                    point
                })
                .collect::<Vec<_>>(),
            stopped: false,
        }
    }

    fn shift_rock_y(&self, index: usize, amount: isize) -> Rock {
        Rock {
            points: self.rocks[index]
                .points
                .iter()
                .cloned()
                .map(|mut point| {
                    point.y = (point.y as isize + amount) as usize;

                    point
                })
                .collect::<Vec<_>>(),
            stopped: false,
        }
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
        let rock_ind = chamber.add_rock(rotation[rock_count % rotation.len()](
            chamber.max_height() + 3,
        ));

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

            //println!("Moving in direction {:?}", dir);
            let shifted = match dir {
                Direction::Left => chamber.shift_rock_x(rock_ind, -1),
                Direction::Right => chamber.shift_rock_x(rock_ind, 1),
            };

            if shifted.min_x() < 0 || shifted.max_x() > 6 {
            } else {
                let overlaps = other_rocks
                    .iter()
                    .find(|rock| shifted.overlaps(rock))
                    .is_some();

                if !overlaps {
                    chamber.rocks[rock_ind] = shifted;
                }
            }

            //println!("After vent:");
            //chamber.display();

            if chamber.rocks[rock_ind].min_y() == 0 {
                chamber.rocks[rock_ind].stopped = true;
            } else {
                let gravity = chamber.shift_rock_y(rock_ind, -1);

                let overlaps = other_rocks
                    .iter()
                    .find(|rock| gravity.overlaps(rock))
                    .is_some();

                if overlaps {
                    println!("Rock stopped");
                    chamber.rocks[rock_ind].stopped = true;
                } else {
                    chamber.rocks[rock_ind] = gravity;
                }
            }

            //println!("After gravity:");
            //chamber.display();
        }

        println!("The {}th rock stopped falling", rock_ind + 1);
    }

    println!("Maximum height: {}", chamber.max_height());
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
