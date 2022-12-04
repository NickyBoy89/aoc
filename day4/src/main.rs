use std::str::FromStr;
use std::{fs::File, io::Read};

#[derive(Debug, Clone, Copy)]
struct SectionRange {
    start: u64,
    end: u64,
}

impl SectionRange {
    fn parse(input: &str) -> SectionRange {
        let parts: Vec<&str> = input.split("-").collect();
        SectionRange {
            start: u64::from_str(parts[0]).unwrap(),
            end: u64::from_str(parts[1]).unwrap(),
        }
    }

    fn overlaps_completely(self, other: SectionRange) -> bool {
        if self.start <= other.start && self.end >= other.end {
            return true;
        } else if other.start <= self.start && other.end >= self.end {
            return true;
        }
        return false;
    }

    fn overlap_partly(self, other: SectionRange) -> bool {
        // Current range starts after the other range
        if self.start > other.end {
            return false;
            // Current range ends before the other range
        } else if self.end < other.start {
            return false;
        }
        return true;
    }
}

#[derive(Debug)]
struct Assignment {
    first: SectionRange,
    second: SectionRange,
}

impl Assignment {
    fn parse(input: &str) -> Assignment {
        let parts: Vec<SectionRange> = input.split(",").map(|x| SectionRange::parse(x)).collect();
        Assignment {
            first: parts[0],
            second: parts[1],
        }
    }
}

fn part1(assignments: &Vec<Assignment>) {
    let mut complete_overlaps = 0;

    for assignment in assignments {
        if assignment.first.overlaps_completely(assignment.second) {
            complete_overlaps += 1;
        }
    }

    println!("{:?}", complete_overlaps);
}

fn part2(assignments: &Vec<Assignment>) {
    let mut overlaps = 0;

    for assignment in assignments {
        if assignment.first.overlap_partly(assignment.second) {
            overlaps += 1;
        }
    }

    println!("{:?}", overlaps);
}

fn main() -> std::io::Result<()> {
    let mut f = File::open("input")?;
    let mut contents = String::new();
    f.read_to_string(&mut contents)?;

    let assignments: Vec<Assignment> = contents.lines().map(|x| Assignment::parse(x)).collect();

    part1(&assignments);
    part2(&assignments);

    Ok(())
}
