use std::{
    collections::{HashMap, HashSet},
    fs,
};

#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
struct Pos {
    x: isize,
    y: isize,
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
enum Facing {
    Up,
    Down,
    Left,
    Right,
}

impl Facing {
    fn turn_right(&self) -> Facing {
        match self {
            Facing::Up => Facing::Right,
            Facing::Right => Facing::Down,
            Facing::Down => Facing::Left,
            Facing::Left => Facing::Up,
        }
    }
}

// NOTE: Assumes no diagonals
fn positions_between(start: &Pos, end: &Pos) -> HashSet<Pos> {
    if start.x == end.x {
        let ys = if start.y > end.y {
            (end.y + 1..=start.y).rev().collect::<Vec<_>>()
        } else {
            (start.y..end.y).collect::<Vec<_>>()
        };

        return HashSet::from_iter(ys.iter().map(|&y| Pos { x: start.x, y }));
    }

    assert_ne!(start.x, end.x);

    let xs = if start.x > end.x {
        (end.x + 1..=start.x).rev().collect::<Vec<_>>()
    } else {
        (start.x..end.x).collect::<Vec<_>>()
    };

    return HashSet::from_iter(xs.iter().map(|&x| Pos { x, y: start.y }));
}

fn grid_has_cycle(grid: Vec<Vec<char>>) -> bool {
    let mut start_pos = Pos { x: 0, y: 0 };

    let mut visited_locations: HashSet<Pos> = HashSet::new();

    let mut by_x: HashMap<isize, Vec<Pos>> = HashMap::new();
    let mut by_y: HashMap<isize, Vec<Pos>> = HashMap::new();

    for (ri, row) in grid.iter().enumerate() {
        for (ci, col) in row.iter().enumerate() {
            let cur_pos = Pos {
                x: ci as isize,
                y: ri as isize,
            };

            if *col == '^' {
                start_pos = cur_pos;
            } else if *col == '#' {
                by_x.entry(cur_pos.x).or_insert(Vec::new());
                by_x.get_mut(&cur_pos.x).unwrap().push(cur_pos);
                by_y.entry(cur_pos.y).or_insert(Vec::new());
                by_y.get_mut(&cur_pos.y).unwrap().push(cur_pos);
            }
        }
    }

    let m = grid.len() as isize;
    let n = grid[0].len() as isize;

    let mut facing = Facing::Up;
    let mut cur_pos = start_pos;

    // let mut history = HashSet::new();
    let mut history = Vec::new();

    loop {
        let state = (facing, cur_pos);
        // println!("History: {:?}", history);
        if history.contains(&state) {
            return true;
        }
        history.push(state);

        match facing {
            Facing::Up | Facing::Down => {
                if !by_x.contains_key(&cur_pos.x) {
                    return false;
                }
            }
            Facing::Left | Facing::Right => {
                if !by_y.contains_key(&cur_pos.y) {
                    return false;
                }
            }
        }

        match facing {
            Facing::Up => {
                let potential = by_x[&cur_pos.x]
                    .iter()
                    .map(|&ob| ob.y)
                    .filter(|&y| y < cur_pos.y)
                    .max();

                let potential = match potential {
                    Some(y) => Some(Pos { x: cur_pos.x, y }),
                    None => None,
                };

                if let Some(obstacle) = potential {
                    let between = positions_between(&cur_pos, &obstacle);
                    visited_locations.extend(&between);
                    cur_pos.y = obstacle.y + 1;
                } else {
                    let edge = Pos {
                        x: cur_pos.x,
                        y: -1,
                    };
                    visited_locations.extend(&positions_between(&cur_pos, &edge));
                    break;
                }
            }
            Facing::Right => {
                let potential = by_y[&cur_pos.y]
                    .iter()
                    .map(|&ob| ob.x)
                    .filter(|&x| x > cur_pos.x)
                    .min();

                let potential = match potential {
                    Some(x) => Some(Pos { x, y: cur_pos.y }),
                    None => None,
                };

                if let Some(obstacle) = potential {
                    let between = positions_between(&cur_pos, &obstacle);
                    visited_locations.extend(&between);
                    cur_pos.x = obstacle.x - 1;
                } else {
                    let edge = Pos {
                        x: n + 1,
                        y: cur_pos.y,
                    };
                    visited_locations.extend(&positions_between(&cur_pos, &edge));
                    break;
                }
            }
            Facing::Down => {
                let potential = by_x[&cur_pos.x]
                    .iter()
                    .map(|&ob| ob.y)
                    .filter(|&y| y > cur_pos.y)
                    .min();

                let potential = match potential {
                    Some(y) => Some(Pos { x: cur_pos.x, y }),
                    None => None,
                };

                if let Some(obstacle) = potential {
                    let between = positions_between(&cur_pos, &obstacle);
                    visited_locations.extend(&between);
                    cur_pos.y = obstacle.y - 1;
                } else {
                    let edge = Pos {
                        x: cur_pos.x,
                        y: m + 1,
                    };
                    visited_locations.extend(&positions_between(&cur_pos, &edge));
                    break;
                }
            }
            Facing::Left => {
                let potential = by_y[&cur_pos.y]
                    .iter()
                    .map(|&ob| ob.x)
                    .filter(|&x| x < cur_pos.x)
                    .max();

                let potential = match potential {
                    Some(x) => Some(Pos { x, y: cur_pos.y }),
                    None => None,
                };

                if let Some(obstacle) = potential {
                    let between = positions_between(&cur_pos, &obstacle);
                    visited_locations.extend(&between);
                    cur_pos.x = obstacle.x + 1;
                } else {
                    let edge = Pos {
                        x: -1,
                        y: cur_pos.y,
                    };
                    visited_locations.extend(&positions_between(&cur_pos, &edge));
                    break;
                }
            }
        }
        facing = facing.turn_right();
    }

    false
}

fn part1(input: &String) {
    let mut start_pos = Pos { x: 0, y: 0 };

    let mut visited_locations: HashSet<Pos> = HashSet::new();

    let mut by_x: HashMap<isize, Vec<Pos>> = HashMap::new();
    let mut by_y: HashMap<isize, Vec<Pos>> = HashMap::new();

    for (ri, row) in input.lines().enumerate() {
        for (ci, col) in row.chars().enumerate() {
            let cur_pos = Pos {
                x: ci as isize,
                y: ri as isize,
            };
            if col == '^' {
                start_pos = cur_pos;
            } else if col == '#' {
                by_x.entry(cur_pos.x).or_insert(Vec::new());
                by_x.get_mut(&cur_pos.x).unwrap().push(cur_pos);
                by_y.entry(cur_pos.y).or_insert(Vec::new());
                by_y.get_mut(&cur_pos.y).unwrap().push(cur_pos);
            }
        }
    }

    let grid = input
        .lines()
        .map(|line| line.chars().collect::<Vec<_>>())
        .collect::<Vec<_>>();

    let m = grid.len() as isize;
    let n = grid[0].len() as isize;

    let mut facing = Facing::Up;
    let mut cur_pos = start_pos;

    println!("m: {:?}, n: {:?}", m, n);

    println!(
        "Start: {:?}, at: {:}",
        start_pos, grid[start_pos.y as usize][start_pos.x as usize]
    );

    // let mut history = HashSet::new();
    let mut history = Vec::new();

    loop {
        let state = (facing, cur_pos);
        if history.contains(&state) {
            println!("History: {:?}", history);
            println!("Cycle with state: {:?}", state);
            panic!("Cycle!");
        }
        history.push(state);
        match facing {
            Facing::Up => {
                let potential = by_x[&cur_pos.x]
                    .iter()
                    .map(|&ob| ob.y)
                    .filter(|&y| y < cur_pos.y)
                    .max();

                let potential = match potential {
                    Some(y) => Some(Pos { x: cur_pos.x, y }),
                    None => None,
                };

                if let Some(obstacle) = potential {
                    let between = positions_between(&cur_pos, &obstacle);
                    visited_locations.extend(&between);
                    cur_pos.y = obstacle.y + 1;
                } else {
                    let edge = Pos {
                        x: cur_pos.x,
                        y: -1,
                    };
                    visited_locations.extend(&positions_between(&cur_pos, &edge));
                    break;
                }
            }
            Facing::Right => {
                let potential = by_y[&cur_pos.y]
                    .iter()
                    .map(|&ob| ob.x)
                    .filter(|&x| x > cur_pos.x)
                    .min();

                let potential = match potential {
                    Some(x) => Some(Pos { x, y: cur_pos.y }),
                    None => None,
                };

                if let Some(obstacle) = potential {
                    let between = positions_between(&cur_pos, &obstacle);
                    visited_locations.extend(&between);
                    cur_pos.x = obstacle.x - 1;
                } else {
                    let edge = Pos {
                        x: n + 1,
                        y: cur_pos.y,
                    };
                    visited_locations.extend(&positions_between(&cur_pos, &edge));
                    break;
                }
            }
            Facing::Down => {
                let potential = by_x[&cur_pos.x]
                    .iter()
                    .map(|&ob| ob.y)
                    .filter(|&y| y > cur_pos.y)
                    .min();

                let potential = match potential {
                    Some(y) => Some(Pos { x: cur_pos.x, y }),
                    None => None,
                };

                if let Some(obstacle) = potential {
                    let between = positions_between(&cur_pos, &obstacle);
                    visited_locations.extend(&between);
                    cur_pos.y = obstacle.y - 1;
                } else {
                    let edge = Pos {
                        x: cur_pos.x,
                        y: m + 1,
                    };
                    visited_locations.extend(&positions_between(&cur_pos, &edge));
                    break;
                }
            }
            Facing::Left => {
                let potential = by_y[&cur_pos.y]
                    .iter()
                    .map(|&ob| ob.x)
                    .filter(|&x| x < cur_pos.x)
                    .max();

                let potential = match potential {
                    Some(x) => Some(Pos { x, y: cur_pos.y }),
                    None => None,
                };

                if let Some(obstacle) = potential {
                    let between = positions_between(&cur_pos, &obstacle);
                    visited_locations.extend(&between);
                    cur_pos.x = obstacle.x + 1;
                } else {
                    let edge = Pos {
                        x: -1,
                        y: cur_pos.y,
                    };
                    visited_locations.extend(&positions_between(&cur_pos, &edge));
                    break;
                }
            }
        }
        facing = facing.turn_right();

        // for (ri, row) in grid.iter().enumerate() {
        //     for (ci, col) in row.iter().enumerate() {
        //         let pos = Pos {
        //             x: ci as isize,
        //             y: ri as isize,
        //         };
        //         // if visited_locations.contains(&pos) {
        //         //     print!("X");
        //         // } else {
        //         //     print!("{:}", col);
        //         // }
        //         if grid[pos.y as usize][pos.x as usize] == '.' && visited_locations.contains(&pos) {
        //             print!("X");
        //         } else {
        //             print!("{:}", col);
        //         }
        //     }
        //     println!();
        // }
        //
        // println!("\n\n\n");
    }

    // for (ri, row) in grid.iter().enumerate() {
    //     for (ci, col) in row.iter().enumerate() {
    //         let pos = Pos {
    //             x: ci as isize,
    //             y: ri as isize,
    //         };
    //         if visited_locations.contains(&pos) {
    //             print!("X");
    //         } else {
    //             print!("{:}", col);
    //         }
    //     }
    //     println!();
    // }

    println!(
        "Guard visited {:} unique locations",
        visited_locations.len()
    );
}

fn pretty_print_grid(grid: &Vec<Vec<char>>) {
    for (ri, row) in grid.iter().enumerate() {
        for (ci, col) in row.iter().enumerate() {
            let pos = Pos {
                x: ci as isize,
                y: ri as isize,
            };
            // if visited_locations.contains(&pos) {
            //     print!("X");
            // } else {
            //     print!("{:}", col);
            // }
            print!("{:}", col);
        }
        println!();
    }
}

fn part2(input: &String) {
    let mut num_options = 0;

    let init_grid = input
        .lines()
        .map(|line| line.chars().collect::<Vec<_>>())
        .collect::<Vec<_>>();

    for (ri, row) in input.lines().enumerate() {
        for (ci, col) in row.chars().enumerate() {
            if col == '^' {
                continue;
            }
            let mut new_grid = init_grid.clone();
            new_grid[ri][ci] = '#';
            // pretty_print_grid(&new_grid);
            // println!("\n\n\n");
            if grid_has_cycle(new_grid) {
                num_options += 1;
            }
        }
    }

    println!("Options: {:?}", num_options);
}

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();

    // part1(&input);
    part2(&input);
}
