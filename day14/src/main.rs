use std::collections::HashSet;
use std::str::FromStr;
use std::{fs::File, io::Read};

#[derive(Debug, Eq, PartialEq, Copy, Clone, Hash)]
struct Point {
    x: isize,
    y: isize,
}

impl Point {
    fn parse(input: &str) -> Point {
        let (x, y) = input.split_once(",").unwrap();
        Point {
            x: isize::from_str(x).unwrap(),
            y: isize::from_str(y).unwrap(),
        }
    }

    fn points_from_to(&self, other: &Point) -> Vec<Point> {
        let xdiff: isize = if self.x == other.x {
            0
        } else if self.x < other.x {
            1
        } else {
            -1
        };

        let ydiff = if self.y == other.y {
            0
        } else if self.y < other.y {
            1
        } else {
            -1
        };

        let mut points = Vec::new();
        let mut cur = *self;
        loop {
            points.push(cur);
            if cur == *other {
                break;
            }
            cur.x += xdiff;
            cur.y += ydiff;
        }

        points
    }
}

fn part1(contents: &mut String) {
    let mut points = HashSet::<Point>::new();

    let mut lowest_y = 0;

    for line in contents.lines() {
        for rock_points in line.split(" -> ").collect::<Vec<&str>>().windows(2) {
            for obstable_point in
                Point::parse(rock_points[0]).points_from_to(&Point::parse(rock_points[1]))
            {
                if obstable_point.y > lowest_y {
                    lowest_y = obstable_point.y;
                }
                points.insert(obstable_point);
            }
        }
    }

    let mut total_sand = 0;

    let mut finished = false;

    while !finished {
        let mut sand_pos = Point { x: 500, y: 0 };
        total_sand += 1;
        //println!("Created one sand for a total of {}", total_sand);
        loop {
            // We can move downwards
            if !points.contains(&Point {
                x: sand_pos.x,
                y: sand_pos.y + 1,
            }) {
                sand_pos.y += 1;
            } else if !points.contains(&Point {
                x: sand_pos.x - 1,
                y: sand_pos.y + 1,
            }) {
                sand_pos.x -= 1;
                sand_pos.y += 1;
            } else if !points.contains(&Point {
                x: sand_pos.x + 1,
                y: sand_pos.y + 1,
            }) {
                sand_pos.x += 1;
                sand_pos.y += 1;
            } else {
                // Settled
                points.insert(sand_pos);
                break;
            }

            if sand_pos.y >= lowest_y {
                println!(
                    "Sand {} fell out of world, leaving {} sand",
                    total_sand,
                    total_sand - 1
                );
                finished = true;
                break;
            }
        }
    }
}

fn part2(contents: &mut String) {
    let mut points = HashSet::<Point>::new();

    let mut lowest_y = 0;

    for line in contents.lines() {
        for rock_points in line.split(" -> ").collect::<Vec<&str>>().windows(2) {
            for obstable_point in
                Point::parse(rock_points[0]).points_from_to(&Point::parse(rock_points[1]))
            {
                if obstable_point.y > lowest_y {
                    lowest_y = obstable_point.y;
                }
                points.insert(obstable_point);
            }
        }
    }

    let mut total_sand = 0;

    let hardfloor = 2 + lowest_y;

    loop {
        let mut sand_pos = Point { x: 500, y: 0 };

        if points.contains(&sand_pos) {
            println!("Source blocked after {} sands", total_sand);
            break;
        }

        total_sand += 1;
        //println!("Created one sand for a total of {}", total_sand);
        loop {
            // We can move downwards
            if !points.contains(&Point {
                x: sand_pos.x,
                y: sand_pos.y + 1,
            }) {
                sand_pos.y += 1;
            } else if !points.contains(&Point {
                x: sand_pos.x - 1,
                y: sand_pos.y + 1,
            }) {
                sand_pos.x -= 1;
                sand_pos.y += 1;
            } else if !points.contains(&Point {
                x: sand_pos.x + 1,
                y: sand_pos.y + 1,
            }) {
                sand_pos.x += 1;
                sand_pos.y += 1;
            } else {
                // Settled
                points.insert(sand_pos);
                break;
            }

            // Resting on the hardfloor
            if sand_pos.y == hardfloor - 1 {
                points.insert(sand_pos);
                break;
            }
        }
    }
}

fn main() -> std::io::Result<()> {
    let mut f = File::open("input")?;
    let mut contents = String::new();
    f.read_to_string(&mut contents)?;

    part1(&mut contents);
    part2(&mut contents);

    Ok(())
}
