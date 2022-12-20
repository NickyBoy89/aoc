mod rocks;

use std::collections::{HashMap, HashSet};
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
    pub y: isize,
}

#[derive(Debug, Clone)]
pub struct Rock {
    pub points: HashSet<Point>,
    stopped: bool,

    xmin: isize,
    xmax: isize,
    ymin: isize,
    ymax: isize,
}

impl Rock {
    pub fn new(points: Vec<Point>) -> Rock {
        let xmin = points.iter().map(|point| point.x).min().unwrap();
        let xmax = points.iter().map(|point| point.x).max().unwrap();
        let ymin = points.iter().map(|point| point.y).min().unwrap();
        let ymax = points.iter().map(|point| point.y).max().unwrap();

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

fn visualize_ground(ground: &[isize; 7]) {
    let max = *ground.iter().max().unwrap();

    for line in 0..=max {
        for ind in 0..7 {
            if ground[ind] == line {
                print!("#");
            } else {
                print!(".");
            }
        }
        println!();
    }
}

fn part2(directions: &Vec<Direction>) {
    // seven units wide
    // gets pushed first, falls second
    // left edge is two units away from the left wall
    // bottom is three units above the highest rock, or floor

    let rotation = Vec::from([line, plus, ell, vline, square]);

    static NUM_ROCKS: usize = 3;
    //static NUM_ROCKS: usize = 1_000_000_000_000;

    let mut direction_ind = 0;

    // stack_top starts at the floor (-1)
    let mut stack_height = 0;

    // stack_top is a map of every column, and its distance from the top of the stack
    let mut stack_top = [-1; 7];

    for rock_count in 0..NUM_ROCKS {
        let mut rock = rotation[rock_count % rotation.len()](stack_height as isize + 3);

        // Start the falling section
        loop {
            // Fetch the next direction
            let direction = &directions[direction_ind % directions.len()];
            direction_ind += 1;

            println!("Rock is at {:?}", rock);

            // Move left or right

            let top_points = HashSet::<Point>::from_iter((0..7).map(|index| Point {
                x: index as isize,
                y: stack_height - stack_top[index],
            }));

            println!("Points to avoid are at: {:?}", top_points);

            // TODO: Add check for moving into another block
            // and check for above the maximum
            println!("Moving in direction {:?}", direction);
            match direction {
                Direction::Left => {
                    // Check for runnning into the walls
                    if rock.xmin > 0 && rock.points.is_disjoint(&top_points) {
                        rock = rock.shift_x(-1);
                    }
                }
                Direction::Right => {
                    if rock.xmax < 6 && rock.points.is_disjoint(&top_points) {
                        rock = rock.shift_x(1);
                    }
                }
            }

            // Move gravity
            println!("Moving down");
            rock = rock.shift_y(-1);

            if rock.stopped {
                println!("Rock settled");
                println!("Height of rock is {}", rock.height());

                // Adjust heights of the ground
                for index in 0..7 {
                    // Find the highest point at that index for the rock
                    let highest_point = rock
                        .points
                        .iter()
                        .filter(|point| point.x == index as isize)
                        .map(|point| point.y)
                        .max();

                    if highest_point.is_none() {
                        continue;
                    }

                    stack_top[index] -= ((highest_point.unwrap() - rock.ymin) + 1) as isize;
                }

                let min_diff = -*stack_top.iter().min().unwrap();
                println!("Min diff: {}", min_diff);
                for ind in 0..7 {
                    stack_top[ind] += min_diff;
                }

                stack_height += min_diff;

                break;
            }

            let settled = (0..7)
                .map(|index| Point {
                    x: index as isize,
                    y: stack_height - stack_top[index],
                })
                .find(|point| rock.points.contains(point))
                .is_some();

            if settled {
                rock.stopped = true;
            }
        }

        println!(
            "Ground texture after rock: {:?}",
            visualize_ground(&stack_top)
        );
    }

    println!("Maximum height: {}", stack_height);
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
