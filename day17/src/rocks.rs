use crate::{Point, Rock};

pub fn line(height: usize) -> Rock {
    Rock {
        points: vec![
            Point { x: 2, y: height },
            Point { x: 3, y: height },
            Point { x: 4, y: height },
            Point { x: 5, y: height },
        ],
        stopped: false,
    }
}

pub fn plus(height: usize) -> Rock {
    Rock {
        points: vec![
            // Bottom center
            Point { x: 3, y: height },
            // Middle row
            Point {
                x: 2,
                y: height + 1,
            },
            Point {
                x: 3,
                y: height + 1,
            },
            Point {
                x: 4,
                y: height + 1,
            },
            // Top center
            Point {
                x: 3,
                y: height + 2,
            },
        ],
        stopped: false,
    }
}

pub fn ell(height: usize) -> Rock {
    Rock {
        points: vec![
            // Bottom of L
            Point { x: 2, y: height },
            Point { x: 3, y: height },
            Point { x: 4, y: height },
            // Handle of L
            Point {
                x: 4,
                y: height + 1,
            },
            Point {
                x: 4,
                y: height + 2,
            },
        ],
        stopped: false,
    }
}

pub fn vline(height: usize) -> Rock {
    Rock {
        points: vec![
            Point { x: 2, y: height },
            Point {
                x: 2,
                y: height + 1,
            },
            Point {
                x: 2,
                y: height + 2,
            },
            Point {
                x: 2,
                y: height + 3,
            },
        ],
        stopped: false,
    }
}

pub fn square(height: usize) -> Rock {
    Rock {
        points: vec![
            Point { x: 2, y: height },
            Point { x: 3, y: height },
            Point {
                x: 2,
                y: height + 1,
            },
            Point {
                x: 3,
                y: height + 1,
            },
        ],
        stopped: false,
    }
}
