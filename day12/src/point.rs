use std::{
    cmp::Ordering,
    collections::HashMap,
    hash::{Hash, Hasher},
};

pub struct Point {
    pub x: usize,
    pub y: usize,
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
    pub fn new(x: usize, y: usize) -> Point {
        Point { x, y, priority: 0 }
    }

    pub fn new_with_priority(x: usize, y: usize, priority: usize) -> Point {
        Point { x, y, priority }
    }

    fn mul(&self, n: usize) -> Point {
        Point::new(self.x * n, self.y * n)
    }

    pub fn neighbors(&self, grid: &HashMap<Point, usize>) -> Vec<Point> {
        let neighbors = vec![
            Point::new(self.x - 1, self.y),
            Point::new(self.x + 1, self.y),
            Point::new(self.x, self.y - 1),
            Point::new(self.x, self.y + 1),
        ];

        let mut valid = Vec::new();
        for nei in neighbors {
            if grid.contains_key(&nei) {
                valid.push(nei);
            }
        }

        valid
    }

    pub fn xdiff(&self, other: &Point) -> usize {
        self.x - other.x
    }

    pub fn ydiff(&self, other: &Point) -> usize {
        self.y - other.y
    }
}
