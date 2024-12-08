use std::collections::HashSet;
use std::fs;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
struct Pos {
    x: isize,
    y: isize,
}

impl Pos {
    fn antinodes_with(&self, other: &Self) -> (Pos, Pos) {
        let ydiff = self.y - other.y;
        let xdiff = self.x - other.x;

        (
            Pos {
                y: self.y + ydiff,
                x: self.x + xdiff,
            },
            Pos {
                y: other.y - ydiff,
                x: other.x - xdiff,
            },
        )
    }

    fn in_bounds(&self, m: usize, n: usize) -> bool {
        (self.x >= 0 && self.x < n as isize) && (self.y >= 0 && self.y < m as isize)
    }

    fn all_antinodes(&self, other: &Self, m: usize, n: usize) -> Vec<Pos> {
        let mut antinodes = Vec::new();

        let ydiff = self.y - other.y;
        let xdiff = self.x - other.x;

        let mut left_node = self.clone();
        left_node.x += xdiff;
        left_node.y += ydiff;

        while left_node.in_bounds(m, n) {
            antinodes.push(left_node);
            left_node.x += xdiff;
            left_node.y += ydiff;
        }

        let mut right_node = self.clone();
        right_node.x -= xdiff;
        right_node.y -= ydiff;

        while right_node.in_bounds(m, n) {
            antinodes.push(right_node);
            right_node.x -= xdiff;
            right_node.y -= ydiff;
        }

        antinodes
    }
}

#[derive(Debug, Clone)]
struct Antenna {
    pos: Pos,
    frequency: char,
}

#[derive(Clone)]
struct Grid {
    data: Vec<Vec<char>>,
    antennas: Vec<Antenna>,
    antinodes: Vec<Pos>,
}

impl Grid {
    fn new(data: Vec<Vec<char>>) -> Self {
        let mut antennas = Vec::new();

        for (ri, row) in data.iter().enumerate() {
            for (ci, col) in row.iter().enumerate() {
                if *col != '.' {
                    antennas.push(Antenna {
                        pos: Pos {
                            x: ci as isize,
                            y: ri as isize,
                        },
                        frequency: *col,
                    });
                }
            }
        }

        Self {
            data,
            antennas,
            antinodes: Vec::new(),
        }
    }

    fn size(&self) -> (usize, usize) {
        (self.data.len(), self.data[0].len())
    }

    fn pretty_print(&self) {
        for (ri, row) in self.data.iter().enumerate() {
            for (ci, c) in row.iter().enumerate() {
                let cur_pos = Pos {
                    x: ci as isize,
                    y: ri as isize,
                };
                if *c == '@' {
                    print!("@");
                } else if *c == '.' && self.antinodes.contains(&cur_pos) {
                    print!("#");
                } else {
                    print!("{}", c);
                }
            }
            println!();
        }
    }
}

fn part1(grid: &Grid) {
    let mut grid = grid.clone();
    for antenna in grid.antennas.iter() {
        for other in grid.antennas.iter() {
            if antenna.frequency == other.frequency && (antenna.pos != other.pos) {
                let (fst, snd) = antenna.pos.antinodes_with(&other.pos);
                grid.antinodes.push(fst);
                grid.antinodes.push(snd);
            }
        }
    }

    let mut valid_antinodes = HashSet::new();

    for antinode in grid.antinodes.iter() {
        if antinode.x < 0 || antinode.x >= grid.data[0].len() as isize {
            continue;
        } else if antinode.y < 0 || antinode.y >= grid.data.len() as isize {
            continue;
        }

        valid_antinodes.insert(antinode);
    }

    grid.pretty_print();

    println!("Unique antinodes: {}", valid_antinodes.len());
}

fn part2(grid: &Grid) {
    let mut grid = grid.clone();

    let (m, n) = grid.size();

    for antenna in grid.antennas.iter() {
        for other in grid.antennas.iter() {
            if antenna.frequency == other.frequency && (antenna.pos != other.pos) {
                let antis = antenna.pos.all_antinodes(&other.pos, m, n);

                for anti in antis {
                    grid.antinodes.push(anti);
                }
            }
        }
    }

    let mut valid_antinodes = HashSet::new();

    for antinode in grid.antinodes.iter() {
        if antinode.x < 0 || antinode.x >= grid.data[0].len() as isize {
            continue;
        } else if antinode.y < 0 || antinode.y >= grid.data.len() as isize {
            continue;
        }

        valid_antinodes.insert(antinode);
    }

    grid.pretty_print();

    println!("Unique antinodes: {}", valid_antinodes.len());
}

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();

    let grid = Grid::new(
        input
            .lines()
            .map(|line| line.chars().collect::<Vec<char>>())
            .collect::<Vec<_>>(),
    );

    part1(&grid);
    part2(&grid);
}
