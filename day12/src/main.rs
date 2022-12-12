mod point;
mod tile;

use std::{
    collections::{BinaryHeap, HashMap},
    fs::File,
    io::Read,
};

use point::Point;
use tile::Tile;

#[derive(Clone, Copy, Eq, PartialEq)]
struct Directions {
    up: bool,
    down: bool,
    left: bool,
    right: bool,
}

pub struct Heightmap {
    grid: Vec<Vec<Tile>>,
    width: usize,
    height: usize,
    start_position: Point,
    end_position: Point,
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
                    start_position = Point::new(ci as isize, ri as isize);
                } else if tile.is_end {
                    end_position = Point::new(ci as isize, ri as isize);
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

    fn tile_at(&self, p: &Point) -> Tile {
        self.grid[p.y as usize][p.x as usize]
    }

    fn cost_from_to(&self, from: &Point, to: &Point) -> isize {
        self.tile_at(from).heightdiff(&self.tile_at(to))
    }

    fn contains_point(&self, p: &Point) -> bool {
        p.x >= 0 && p.x < self.width as isize && p.y >= 0 && p.y < self.height as isize
    }

    fn d(&self) -> usize {
        let source = &self.start_position;
        let target = &self.end_position;

        let mut frontier = BinaryHeap::from([source.clone()]);

        let mut came_from = HashMap::<Point, Point>::new();
        let mut cost_so_far = HashMap::<Point, usize>::new();

        came_from.insert(source.clone(), Point::new(-1, -1));
        cost_so_far.insert(source.clone(), 0);

        while frontier.len() > 0 {
            let current = frontier.pop().unwrap();
            if current == *target {
                return current.priority;
            }

            for next in current.neighbors(self) {
                let new_cost = cost_so_far[&current] + 1;
                if !cost_so_far.contains_key(&next) || new_cost < cost_so_far[&next] {
                    cost_so_far.insert(next.clone(), new_cost);
                    let item = Point::new_with_priority(next.x, next.y, new_cost);
                    frontier.push(item);
                    came_from.insert(next.clone(), current.clone());
                }
            }
        }

        panic!("No path");
    }
}

fn part1(contents: &mut String) {
    let mut heightmap = Heightmap::parse(contents);

    println!("{}", heightmap.d());
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
