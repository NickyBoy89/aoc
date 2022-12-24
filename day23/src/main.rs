use std::collections::{HashSet, VecDeque};
use std::{fs::File, io::Read};

#[derive(Debug, Hash, Eq, PartialEq, Clone, Copy)]
struct Point {
    x: isize,
    y: isize,
}

#[derive(Debug)]
struct Adjacent {
    n: bool,
    ne: bool,
    e: bool,
    se: bool,
    s: bool,
    sw: bool,
    w: bool,
    nw: bool,
}

impl Adjacent {
    fn blank(&self) -> bool {
        !self.n && !self.ne && !self.e && !self.se && !self.s && !self.sw && !self.w && !self.nw
    }
}

#[derive(Debug)]
struct Ground {
    elves: Vec<Point>,
}

impl Ground {
    fn new() -> Ground {
        Ground { elves: Vec::new() }
    }

    fn has_neighbors(&self, point: &Point) -> bool {
        for ri in point.y - 1..point.y + 1 {
            for ci in point.x - 1..point.x + 1 {
                if point.y == ri && point.x == ci {
                    continue;
                }
                if self.elves.contains(&Point { x: ci, y: ri }) {
                    return false;
                }
            }
        }
        true
    }

    fn adjacents(&self, point: &Point) -> Adjacent {
        let n = self.elves.contains(&Point {
            x: point.x,
            y: point.y - 1,
        });
        let ne = self.elves.contains(&Point {
            x: point.x + 1,
            y: point.y - 1,
        });
        let e = self.elves.contains(&Point {
            x: point.x + 1,
            y: point.y,
        });
        let se = self.elves.contains(&Point {
            x: point.x + 1,
            y: point.y + 1,
        });
        let s = self.elves.contains(&Point {
            x: point.x,
            y: point.y + 1,
        });
        let sw = self.elves.contains(&Point {
            x: point.x - 1,
            y: point.y + 1,
        });
        let w = self.elves.contains(&Point {
            x: point.x - 1,
            y: point.y,
        });
        let nw = self.elves.contains(&Point {
            x: point.x - 1,
            y: point.y - 1,
        });

        Adjacent {
            n,
            ne,
            e,
            se,
            s,
            sw,
            w,
            nw,
        }
    }

    fn display(&self) {
        let max_x = self.elves.iter().map(|elf| elf.x).max().unwrap();
        let min_x = self.elves.iter().map(|elf| elf.x).min().unwrap();
        let max_y = self.elves.iter().map(|elf| elf.y).max().unwrap();
        let min_y = self.elves.iter().map(|elf| elf.y).min().unwrap();

        for row in min_y..=max_y {
            for col in min_x..=max_x {
                if self.elves.contains(&Point { x: col, y: row }) {
                    print!("#");
                } else {
                    print!(".");
                }
            }
            println!();
        }
    }
}

#[derive(Debug)]
enum Direction {
    North,
    South,
    East,
    West,
}

fn part1(contents: &mut String) {
    let mut ground = Ground::new();

    for (ri, row) in contents.lines().enumerate() {
        for (ci, col) in row.chars().enumerate() {
            if col == '#' {
                ground.elves.push(Point {
                    x: ci as isize,
                    y: ri as isize,
                });
            }
        }
    }

    let mut directions = VecDeque::from([
        Direction::North,
        Direction::South,
        Direction::West,
        Direction::East,
    ]);

    static NUM_ROUNDS: usize = 2;
    for round in 0..NUM_ROUNDS {
        println!("Start of proposals");
        println!("Ground: {:?}", ground);

        ground.display();

        let mut proposals = Vec::<Option<Point>>::new();

        for elf in ground.elves.iter() {
            let adjacent = ground.adjacents(&elf);
            if adjacent.blank() {
                println!("No neighbors");
                proposals.push(None);
                continue;
            }
            println!("Adjacent to point: {:?}: {:?}", elf, adjacent);

            for (ind, direction) in directions.iter().enumerate() {
                match direction {
                    Direction::North => {
                        if !adjacent.n && !adjacent.ne && !adjacent.nw {
                            proposals.push(Some(Point {
                                x: elf.x,
                                y: elf.y - 1,
                            }));
                            break;
                        }
                    }
                    Direction::South => {
                        if !adjacent.s && !adjacent.se && !adjacent.sw {
                            proposals.push(Some(Point {
                                x: elf.x,
                                y: elf.y + 1,
                            }));
                            break;
                        }
                    }
                    Direction::West => {
                        if !adjacent.w && !adjacent.nw && !adjacent.sw {
                            proposals.push(Some(Point {
                                x: elf.x - 1,
                                y: elf.y,
                            }));
                            break;
                        }
                    }
                    Direction::East => {
                        if !adjacent.e && !adjacent.ne && !adjacent.se {
                            proposals.push(Some(Point {
                                x: elf.x + 1,
                                y: elf.y,
                            }));
                            break;
                        }
                    }
                };
            }

            directions.rotate_left(1);
        }

        assert!(proposals.len() == ground.elves.len());

        println!("Proposals: {:?}", proposals);

        for (elf_ind, proposal) in proposals.iter().enumerate() {
            if proposals.iter().filter(|prop| *prop == proposal).count() > 1 {
                continue;
            }

            if let Some(new_point) = proposal {
                ground.elves[elf_ind] = *new_point;
            }
        }

        println!("Points are now: {:?}", ground.elves);
        ground.display();
    }

    println!("Ground: {:?}", ground);
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
