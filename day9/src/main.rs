use std::str::FromStr;
use std::{collections::HashSet, fs::File, io::Read};

#[derive(Debug, Eq, Hash, PartialEq, Clone)]
struct Point {
    x: isize,
    y: isize,
}

impl Point {
    fn new(x: isize, y: isize) -> Point {
        Point { x, y }
    }

    fn distance_to(&self, other: &Point) -> (isize, isize) {
        return (other.x - self.x, other.y - self.y);
    }
}

fn part1(contents: &mut String) {
    let mut head = Point::new(0, 0);
    let mut tail = Point::new(0, 0);

    let mut path = HashSet::<Point>::new();

    for line in contents.lines() {
        let (direction, size) = line.split_once(" ").unwrap();

        for _ in 0..usize::from_str(size).unwrap() {
            match direction {
                "L" => head.x -= 1,
                "R" => head.x += 1,
                "D" => head.y -= 1,
                "U" => head.y += 1,
                _ => panic!("Unknown direction command"),
            }

            let (xdiff, ydiff) = tail.distance_to(&head);

            if xdiff.abs() > 1 {
                tail.x += xdiff / 2;
                if ydiff.abs() == 1 {
                    tail.y += ydiff;
                }
            }

            if ydiff.abs() > 1 {
                tail.y += ydiff / 2;
                if xdiff.abs() == 1 {
                    tail.x += xdiff;
                }
            }

            path.insert(tail.to_owned());
        }
    }

    println!("{:?}", path.len());
}

fn part2(contents: &mut String) {
    let mut head = Point::new(0, 0);
    let mut tail = vec![Point::new(0, 0); 9];

    let mut path = HashSet::<Point>::new();

    for line in contents.lines() {
        let (direction, size) = line.split_once(" ").unwrap();

        for _ in 0..usize::from_str(size).unwrap() {
            match direction {
                "L" => head.x -= 1,
                "R" => head.x += 1,
                "D" => head.y -= 1,
                "U" => head.y += 1,
                _ => panic!("Unknown direction command"),
            }

            let mut last = &mut head;

            for mut knot in tail.iter_mut() {
                let (xdiff, ydiff) = knot.distance_to(last);

                if xdiff.abs() > 1 {
                    knot.x += xdiff / 2;
                    if ydiff.abs() == 1 {
                        knot.y += ydiff;
                    }
                }

                if ydiff.abs() > 1 {
                    knot.y += ydiff / 2;
                    if xdiff.abs() == 1 {
                        knot.x += xdiff;
                    }
                }

                last = knot;
            }

            path.insert(tail[8].to_owned());
        }
    }

    println!("{:?}", path.len());
}

fn main() -> std::io::Result<()> {
    let mut f = File::open("input")?;
    let mut contents = String::new();
    f.read_to_string(&mut contents)?;

    part1(&mut contents);
    part2(&mut contents);

    Ok(())
}
