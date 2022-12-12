use std::{
    cmp::Ordering,
    hash::{Hash, Hasher},
};

use crate::Heightmap;

#[derive(Clone, Debug)]
pub struct Point {
    pub x: isize,
    pub y: isize,
    pub priority: usize,
}

impl PartialEq for Point {
    fn eq(&self, other: &Self) -> bool {
        self.x == other.x && self.y == other.y
    }
}

impl Eq for Point {}

impl Hash for Point {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.x.hash(state);
        self.y.hash(state);
    }
}

impl Ord for Point {
    fn cmp(&self, other: &Self) -> Ordering {
        self.priority.cmp(&other.priority)
    }
}

impl PartialOrd for Point {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Point {
    pub fn new(x: isize, y: isize) -> Point {
        Point { x, y, priority: 0 }
    }

    pub fn new_with_priority(x: isize, y: isize, priority: usize) -> Point {
        Point { x, y, priority }
    }

    pub fn neighbors(&self, grid: &Heightmap) -> Vec<Point> {
        let neighbors = vec![
            Point::new(self.x - 1, self.y),
            Point::new(self.x + 1, self.y),
            Point::new(self.x, self.y - 1),
            Point::new(self.x, self.y + 1),
        ];

        let mut valid = Vec::new();
        for nei in neighbors {
            if grid.contains_point(&nei) {
                let cost = grid.cost_from_to(&self, &nei);
                if cost < 2 {
                    valid.push(nei);
                }
            }
        }

        println!("{:?} can go to {:?}", self, valid);

        valid
    }
}
