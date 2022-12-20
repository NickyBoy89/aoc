mod rocks;
mod stack_top;

use std::collections::{HashMap, HashSet};
use std::{fs::File, io::Read};

use rocks::*;
use stack_top::StackTop;

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
    pub y: isize,
}

#[derive(Debug, Clone)]
pub struct Rock {
    pub points: HashSet<Point>,
    stopped: bool,

    pub xmin: isize,
    pub xmax: isize,
    pub ymin: isize,
    pub ymax: isize,
}

impl Rock {
    pub fn new(points: Vec<Point>, xmin: isize, xmax: isize, ymin: isize, ymax: isize) -> Rock {
        Rock {
            points: HashSet::from_iter(points.into_iter()),
            stopped: false,
            xmin,
            xmax,
            ymin,
            ymax,
        }
    }

    fn shift_x(&self, amount: isize) -> Rock {
        let mut new_rock = self.clone();
        new_rock.points = HashSet::from_iter(new_rock.points.drain().map(|mut point| {
            point.x += amount;
            point
        }));
        new_rock.xmin += amount;
        new_rock.xmax += amount;

        new_rock
    }

    fn shift_y(&self, amount: isize) -> Rock {
        let mut new_rock = self.clone();

        new_rock.points = HashSet::from_iter(new_rock.points.drain().map(|mut point| {
            point.y += amount;

            point
        }));
        new_rock.ymin += amount;
        new_rock.ymax += amount;

        new_rock
    }

    fn overlaps(&self, other: &Rock) -> bool {
        !self.points.is_disjoint(&other.points)
    }

    fn height(&self) -> usize {
        ((self.ymax - self.ymin) + 1) as usize
    }

    fn min_at(&self, x: isize) -> Option<isize> {
        self.points
            .iter()
            .filter(|point| point.x == x)
            .map(|point| point.y)
            .min()
    }

    fn max_at(&self, x: isize) -> Option<isize> {
        self.points
            .iter()
            .filter(|point| point.x == x)
            .map(|point| point.y)
            .max()
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
        self.highest = (rock.ymax + 1) as usize;
        self.rocks.push(rock.clone());
        self.rocks.sort();
        self.rocks.iter().position(|r| *r == rock).unwrap()
    }

    fn recalculate_highest(&mut self) {
        self.highest = self.rocks.iter().map(|rock| rock.ymax).max().unwrap() as usize + 1
    }

    fn display(&self) {
        let points = HashSet::<Point>::from_iter(
            self.rocks.iter().map(|rock| rock.points.clone()).flatten(),
        );

        for ri in (0..=self.highest).rev() {
            for ci in 0..7 {
                if points.contains(&Point {
                    x: ci,
                    y: ri as isize,
                }) {
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

fn part1(directions: &Vec<Direction>) {
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
            chamber.highest as isize + 3,
        ));

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

            if shifted.xmin >= 0 && shifted.xmax < 7 {
                let overlaps = other_rocks
                    .iter()
                    .find(|rock| shifted.overlaps(rock))
                    .is_some();

                if !overlaps {
                    chamber.rocks[rock_ind] = shifted;
                }
            }

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
        }
    }

    println!("Maximum height: {}", chamber.highest);
}

fn part2(directions: &Vec<Direction>) {
    let mut rotation = [line, plus, ell, vline, square].iter().cycle();
    let mut directions = directions.iter().cycle();

    static NUM_ROCKS: usize = 2022;
    //static NUM_ROCKS: usize = 1_000_000_000_000;

    // stack_top is a map of every column, and its distance from the top of the stack
    let mut stack_top = StackTop::new();

    stack_top.visualize();

    for rock_count in 0..NUM_ROCKS {
        let mut rock = rotation.next().unwrap()(stack_top.height + 4);

        loop {
            let direction = directions.next().unwrap();

            //println!("Moving in direction {:?}", direction);

            match direction {
                Direction::Left => {
                    if rock.xmin > 0 {
                        let shifted = rock.shift_x(-1);
                        if !stack_top.overlaps_rock(&shifted) {
                            rock = shifted;
                        }
                    }
                }
                Direction::Right => {
                    if rock.xmax < 6 {
                        let shifted = rock.shift_x(1);
                        if !stack_top.overlaps_rock(&shifted) {
                            rock = shifted;
                        }
                    }
                }
            }

            //println!("Rock is at {:?}", rock);

            if rock.points.iter().find(|point| point.y < 0).is_some() {
                panic!("Something slipped through");
            }

            //stack_top.visualize_with_rock(&rock);

            let gravity = rock.shift_y(-1);
            if stack_top.overlaps_rock(&gravity) {
                for (ind, col) in stack_top.top.iter_mut().enumerate() {
                    let max_col = rock.max_at(ind as isize);

                    if let Some(max) = max_col {
                        let new_height = stack_top.height - max;
                        *col = new_height;
                    }
                }

                stack_top.normalize_heights();

                //println!("Settled!");
                //stack_top.visualize();
                break;
            } else {
                rock = gravity;
            }
        }

        println!("Finished rock {}", rock_count + 1);
    }

    println!("Maximum height: {}", stack_top.height);
}

fn main() -> std::io::Result<()> {
    let mut f = File::open("input")?;
    let mut contents = String::new();
    f.read_to_string(&mut contents)?;

    let directions = contents
        .chars()
        .map(Direction::parse)
        .filter(|d| d.is_some())
        .map(|d| d.unwrap())
        .collect::<Vec<_>>();

    //part1(&directions);
    part2(&directions);

    Ok(())
}
