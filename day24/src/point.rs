use std::{
    cmp::Ordering,
    hash::{Hash, Hasher},
};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Point {
    pub x: isize,
    pub y: isize,
    pub time_elapsed: usize,
}

impl Hash for Point {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.x.hash(state);
        self.y.hash(state);
    }
}

impl Ord for Point {
    fn cmp(&self, other: &Self) -> Ordering {
        other.time_elapsed.cmp(&self.time_elapsed)
    }
}

impl PartialOrd for Point {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Point {
    pub fn new(x: isize, y: isize) -> Point {
        Point {
            x,
            y,
            time_elapsed: 0,
        }
    }

    pub fn neighbors(&self) -> Vec<Point> {
        vec![
            Point::new(self.x - 1, self.y),
            Point::new(self.x + 1, self.y),
            Point::new(self.x, self.y - 1),
            Point::new(self.x, self.y + 1),
        ]
    }
}
