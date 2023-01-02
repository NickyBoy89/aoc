#![feature(binary_heap_retain)]

mod point;
use point::Point;
use std::collections::{BinaryHeap, HashMap, HashSet, VecDeque};
use std::{fs::File, io::Read};

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
enum Direction {
    Up,
    Right,
    Down,
    Left,
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
struct Blizzard {
    pos: Point,
    facing: Direction,
}

impl Blizzard {
    fn move_x(&self, amount: isize) -> Blizzard {
        let mut new_blizz = self.clone();
        new_blizz.pos.x += amount;
        new_blizz
    }

    fn move_y(&self, amount: isize) -> Blizzard {
        let mut new_blizz = self.clone();
        new_blizz.pos.y += amount;
        new_blizz
    }

    fn set_x(&self, amount: isize) -> Blizzard {
        let mut new_blizz = self.clone();
        new_blizz.pos.x = amount;
        new_blizz
    }

    fn set_y(&self, amount: isize) -> Blizzard {
        let mut new_blizz = self.clone();
        new_blizz.pos.y = amount;
        new_blizz
    }
}

#[derive(Clone, Hash, Eq, PartialEq)]
struct ExtractionArea {
    pos: Point,
    width: usize,
    height: usize,
    walls: Vec<Point>,
    blizzards: Vec<Blizzard>,
}

impl ExtractionArea {
    fn display(&self) {
        for row in 0..self.height {
            for col in 0..self.width {
                let point = Point::new(col as isize, row as isize);
                if self.walls.contains(&point) {
                    print!("#");
                    continue;
                }

                let mut found = false;
                for blizzard in self.blizzards.iter() {
                    if blizzard.pos == point {
                        found = true;
                        match blizzard.facing {
                            Direction::Up => print!("^"),
                            Direction::Right => print!(">"),
                            Direction::Down => print!("v"),
                            Direction::Left => print!("<"),
                        }
                        break;
                    }
                }

                if !found {
                    print!(".");
                }
            }
            println!();
        }
    }

    fn move_storms(&mut self) {
        let cantidates = self
            .blizzards
            .iter()
            .map(|blizz| match blizz.facing {
                Direction::Up => blizz.move_y(-1),
                Direction::Down => blizz.move_y(1),
                Direction::Right => blizz.move_x(1),
                Direction::Left => blizz.move_x(-1),
            })
            .collect::<Vec<_>>();
        self.blizzards.clear();
        for cantidate in cantidates {
            if self.walls.contains(&cantidate.pos) {
                self.blizzards.push(match cantidate.facing {
                    Direction::Up => cantidate.set_y(self.height as isize - 2),
                    Direction::Down => cantidate.set_y(1),
                    Direction::Right => cantidate.set_x(1),
                    Direction::Left => cantidate.set_x(self.width as isize - 2),
                });
            } else {
                self.blizzards.push(cantidate);
            }
        }
    }

    fn point_in_wall(&self, point: &Point) -> bool {
        for wall in self.walls.iter() {
            if wall.x == point.x && wall.y == point.y {
                return true;
            }
        }
        false
    }

    fn point_in_blizzard(&self, point: &Point) -> bool {
        for blizz in self.blizzards.iter() {
            if blizz.pos.x == point.x && blizz.pos.y == point.y {
                return true;
            }
        }
        false
    }
}

/*
impl PartialEq for ExtractionArea {
    fn eq(&self, other: &Self) -> bool {
        unimplemented!()
    }
}

impl Eq for ExtractionArea {}
*/

impl Ord for ExtractionArea {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.pos.cmp(&other.pos)
    }
}

impl PartialOrd for ExtractionArea {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

fn part1(contents: &mut String) {
    let lines = contents.lines().collect::<Vec<_>>();
    let width = lines[0].len();
    let height = lines.len();

    let mut walls = Vec::new();
    let mut blizzards = Vec::new();

    for (ri, line) in lines.iter().enumerate() {
        for (ci, tile) in line.chars().enumerate() {
            let cur_point = Point::new(ci as isize, ri as isize);

            match tile {
                '#' => {
                    walls.push(cur_point);
                }
                '^' => {
                    blizzards.push(Blizzard {
                        pos: cur_point,
                        facing: Direction::Up,
                    });
                }
                '>' => {
                    blizzards.push(Blizzard {
                        pos: cur_point,
                        facing: Direction::Right,
                    });
                }
                'v' => {
                    blizzards.push(Blizzard {
                        pos: cur_point,
                        facing: Direction::Down,
                    });
                }
                '<' => {
                    blizzards.push(Blizzard {
                        pos: cur_point,
                        facing: Direction::Left,
                    });
                }
                '.' => (),
                other => panic!("Unknown tile: {}", other),
            }
        }
    }

    let start_point = Point::new(1, 0);
    let end_point = Point::new(width as isize - 2, height as isize - 1);

    println!("Starting at {:?}, ending at {:?}", start_point, end_point);

    let start_area = ExtractionArea {
        pos: start_point,
        width,
        height,
        walls,
        blizzards,
    };

    start_area.display();

    let mut history = HashSet::<ExtractionArea>::new();

    let mut lowest = usize::MAX;

    let mut states = BinaryHeap::from([start_area]);
    while !states.is_empty() {
        // Get the current state
        let mut current = states.pop().unwrap();
        current.move_storms();

        if current.pos.time_elapsed > lowest {
            continue;
        }

        if !history.insert(current.clone()) {
            continue;
        }

        // If we are on the target
        if current.pos.x == end_point.x && current.pos.y == end_point.y {
            if current.pos.time_elapsed < lowest {
                lowest = current.pos.time_elapsed;
                history.retain(|state| state.pos.time_elapsed <= lowest);
                states.retain(|state| state.pos.time_elapsed <= lowest);
            }
        }

        // Since we can always wait, unless there is a blizzard currently
        if !current.point_in_blizzard(&current.pos) {
            let mut wait = current.clone();
            wait.pos.time_elapsed += 1;
            states.push(wait);
        }

        // Explore all the other options
        for mut other in current.pos.neighbors() {
            // Make sure the new point is not in any walls or blizzards
            if current.point_in_wall(&other) || current.point_in_blizzard(&other) {
                continue;
            }

            if other.x < 0 || other.x >= current.width as isize {
                continue;
            } else if other.y < 0 || other.y >= current.height as isize {
                continue;
            }

            let mut new_state = current.clone();
            other.time_elapsed = current.pos.time_elapsed + 1;
            new_state.pos = other;
            states.push(new_state);
        }
    }

    println!("Lowest: {}", lowest);
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
