use std::collections::{HashMap, HashSet};

use crate::{Point, Rock};

#[derive(Debug)]
pub struct StackTop {
    pub height: isize,
    pub top: [isize; 7],
    pub holes: HashMap<isize, Vec<(isize, isize)>>,
}

impl StackTop {
    pub fn new() -> StackTop {
        StackTop {
            top: [0; 7],
            height: 0,
            holes: HashMap::new(),
        }
    }

    pub fn points(&self) -> Vec<Point> {
        let mut points = Vec::new();

        for (ind, col) in self.top.iter().enumerate() {
            points.push(Point {
                x: ind as isize,
                y: self.height - col,
            });
        }

        points
    }

    pub fn visualize(&self) {
        println!("Starting height is {}", self.height);
        let max = *self.top.iter().max().unwrap();
        let digits = max.to_string().len();

        for line in 0..=max {
            print!("{}{}", line, " ".repeat(digits - line.to_string().len()));
            for ind in 0..7 {
                if self.top[ind] == line {
                    print!("#");
                } else {
                    print!(".");
                }
            }
            println!();
        }
    }

    pub fn visualize_with_rock(&self, rock: &Rock) {
        println!("Starting height is {}", self.height);
        let max = *self.top.iter().max().unwrap();
        let digits = max.to_string().len();

        for line in 0..=max {
            print!("{}{}", line, " ".repeat(digits - line.to_string().len()));
            for ind in 0..7 {
                if self.top[ind] == line {
                    print!("#");
                } else if rock.points.contains(&Point {
                    x: ind as isize,
                    y: self.height - line,
                }) {
                    print!("@");
                } else {
                    print!(".");
                }
            }
            println!();
        }
    }

    pub fn normalize_heights(&mut self) {
        //println!("Before normalization: {:?}", self.top);
        let min = -*self.top.iter().min().unwrap();

        self.height += min;

        for height in self.top.iter_mut() {
            *height += min;
        }

        //println!("After, is: {:?}", self.top);
    }

    pub fn overlaps_rock(&self, rock: &Rock) -> bool {
        //println!("Stack is: {:?}", self);
        let stack_top_points = HashSet::from_iter(self.points().into_iter());
        if !stack_top_points.is_disjoint(&rock.points) {
            return true;
        }

        for point in rock.points.iter() {
            let point_height = self.height - self.top[point.x as usize];
            if point.y < point_height {
                return true;
            }
        }

        false
    }
}
