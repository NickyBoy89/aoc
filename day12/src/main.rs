mod point;

use std::{
    collections::{BinaryHeap, HashMap},
    fs::File,
    io::Read,
};

use point::Point;

/*
fn d(source: Point, target: Point, grid: &HashMap<Point, usize>) -> usize {
    let mut frontier = BinaryHeap::from([source]);

    let mut came_from = HashMap::<Point, Point>::new();
    let mut cost_so_far = HashMap::<Point, usize>::new();

    came_from.insert(source, Point::new(0, 0));
    cost_so_far.insert(source, 0);

    while frontier.len() > 0 {
        let current = frontier.pop().unwrap();
        if current == target {
            return current.priority;
        }

        for next in current.neighbors(grid) {
            let new_cost = cost_so_far[&current] + grid[&next];
            if cost_so_far.contains_key(&next) || new_cost < cost_so_far[&next] {
                cost_so_far[&next] = new_cost;
                let item = Point::new_with_priority(next.x, next.y, new_cost);
                frontier.push(item);
                came_from[&next] = current;
            }
        }
    }

    panic!("No path");
}
*/

#[derive(Clone, Copy, Eq, PartialEq)]
struct Tile {
    height: usize,
    explored: Directions,
    is_start: bool,
    is_end: bool,
}

impl Tile {
    fn blank() -> Tile {
        Tile {
            height: 0,
            explored: Directions {
                up: false,
                down: false,
                left: false,
                right: false,
            },
            is_start: false,
            is_end: false,
        }
    }
    fn parse(input: char) -> Tile {
        if input == 'S' {
            return Tile {
                height: 0,
                is_start: true,
                is_end: false,
                explored: Directions {
                    up: false,
                    down: false,
                    left: false,
                    right: false,
                },
            };
        } else if input == 'E' {
            return Tile {
                height: 25,
                is_start: false,
                is_end: true,
                explored: Directions {
                    up: false,
                    down: false,
                    left: false,
                    right: false,
                },
            };
        }
        Tile {
            height: input as usize - 'a' as usize,
            is_start: false,
            is_end: false,
            explored: Directions {
                up: false,
                down: false,
                left: false,
                right: false,
            },
        }
    }

    fn heightdiff(&self, other: &Tile) -> usize {
        other.height - self.height
    }

    fn explored(&self) -> bool {
        self.explored.up == true
            && self.explored.down == true
            && self.explored.left == true
            && self.explored.right == true
    }
}

#[derive(Clone, Copy, Eq, PartialEq)]
struct Directions {
    up: bool,
    down: bool,
    left: bool,
    right: bool,
}

struct Heightmap {
    grid: Vec<Vec<Tile>>,
    width: usize,
    height: usize,
    start_position: Point,
    end_position: Point,
}

fn to_height(c: char) -> usize {
    c as usize - 'a' as usize
}

impl Heightmap {
    fn parse(input: &str) -> Heightmap {
        let lines = input.lines().collect::<Vec<_>>();
        let height = lines.len();
        let width = lines[0].len();

        let mut grid = vec![vec![Tile::blank(); width]; height];

        let mut start_position = Point::new(0, 0);
        let mut end_position = Point::new(0, 0);

        for (ri, line) in input.lines().enumerate() {
            for (ci, c) in line.chars().enumerate() {
                let tile = Tile::parse(c);
                if tile.is_start {
                    start_position = Point::new(ri, ci);
                } else if tile.is_end {
                    end_position = Point::new(ri, ci);
                }

                grid[ri][ci] = tile;
            }
        }
        let grid = input
            .lines()
            .map(|line| line.chars())
            .map(|chars| chars.map(Tile::parse).collect::<Vec<_>>())
            .collect::<Vec<Vec<_>>>();

        Heightmap {
            grid,
            width,
            height,
            start_position,
            end_position,
        }
    }

    fn movement_options(&self, p: &Point) -> Directions {
        let up = if p.y == 0 {
            false
        } else {
            self.grid[p.y][p.x].heightdiff(&self.grid[p.y - 1][p.x]) <= 1
        };

        let down = if p.y == self.height - 1 {
            false
        } else {
            self.grid[p.y][p.x].heightdiff(&self.grid[p.y + 1][p.x]) <= 1
        };

        let left = if p.x == 0 {
            false
        } else {
            self.grid[p.y][p.x].heightdiff(&self.grid[p.y][p.x - 1]) <= 1
        };

        let right = if p.x == self.width - 1 {
            false
        } else {
            self.grid[p.y][p.x].heightdiff(&self.grid[p.y][p.x - 1]) <= 1
        };

        Directions {
            up,
            down,
            left,
            right,
        }
    }

    fn tile_at(&self, p: &Point) -> Tile {
        self.grid[p.x][p.y]
    }

    fn cost_from_to(&self, from: &Point, to: &Point) -> usize {
        let heightdiff = self.tile_at(from).heightdiff(&self.tile_at(to));

        if heightdiff < 0 {
            return 0;
        }
        heightdiff
    }
}

fn part1(contents: &mut String) {
    let mut heightmap = Heightmap::parse(contents);

    /*
    println!(
        "{}",
        d(heightmap.start_position, heightmap.end_position, heightmap)
    );
    */
}

fn part2(contents: &mut String) {}

fn main() -> std::io::Result<()> {
    let mut f = File::open("input")?;
    let mut contents = String::new();
    f.read_to_string(&mut contents)?;

    part1(&mut contents);
    //part2(&mut contents);

    Ok(())
}
