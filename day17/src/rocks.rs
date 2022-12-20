use crate::{Point, Rock};

pub fn line(height: isize) -> Rock {
    Rock::new(vec![
        Point { x: 2, y: height },
        Point { x: 3, y: height },
        Point { x: 4, y: height },
        Point { x: 5, y: height },
    ])
}

pub fn plus(height: isize) -> Rock {
    Rock::new(vec![
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
    ])
}

pub fn ell(height: isize) -> Rock {
    Rock::new(vec![
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
    ])
}

pub fn vline(height: isize) -> Rock {
    Rock::new(vec![
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
    ])
}

pub fn square(height: isize) -> Rock {
    Rock::new(vec![
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
    ])
}
