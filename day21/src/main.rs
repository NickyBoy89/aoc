use std::collections::HashMap;
use std::str::FromStr;
use std::{fs::File, io::Read};

#[derive(Debug, Clone)]
enum Operation {
    Add,
    Subtract,
    Multiply,
    Divide,
}

impl Operation {
    fn parse(input: char) -> Operation {
        match input {
            '+' => Operation::Add,
            '-' => Operation::Subtract,
            '*' => Operation::Multiply,
            '/' => Operation::Divide,
            c => panic!("Unknown operation: {}", c),
        }
    }

    fn inverse(&self) -> Operation {
        match self {
            Operation::Add => Operation::Subtract,
            Operation::Subtract => Operation::Add,
            Operation::Multiply => Operation::Divide,
            Operation::Divide => Operation::Multiply,
        }
    }
}

#[derive(Debug, Clone)]
enum MonkeyNumber {
    Resolved(isize),
    Unresolved(String, Operation, String),
}

impl MonkeyNumber {
    fn is_resolved(&self) -> bool {
        matches!(*self, MonkeyNumber::Resolved(_))
    }

    fn has_child_resolved(&self, numbers: &HashMap<&str, MonkeyNumber>) -> bool {
        if let MonkeyNumber::Unresolved(left_child, _, right_child) = self {
            return numbers[left_child.as_str()].is_resolved()
                || numbers[right_child.as_str()].is_resolved();
        }
        panic!("Number was not unresolved");
    }

    fn left_resolved(&self, numbers: &HashMap<&str, MonkeyNumber>) -> bool {
        if let MonkeyNumber::Unresolved(left_child, _, _) = self {
            return numbers[left_child.as_str()].is_resolved();
        }
        panic!("Number was not unresolved");
    }

    fn right_resolved(&self, numbers: &HashMap<&str, MonkeyNumber>) -> bool {
        if let MonkeyNumber::Unresolved(_, _, right_child) = self {
            return numbers[right_child.as_str()].is_resolved();
        }
        panic!("Number was not unresolved");
    }

    fn inverse(&self) -> MonkeyNumber {
        match self {
            MonkeyNumber::Resolved(num) => MonkeyNumber::Resolved(*num),
            MonkeyNumber::Unresolved(left, op, right) => {
                MonkeyNumber::Unresolved(left.to_string(), op.inverse(), right.to_string())
            }
        }
    }
}

fn part1(contents: &mut String) {
    let mut numbers = HashMap::<&str, MonkeyNumber>::new();
    for line in contents.lines() {
        let (name, expr) = line.split_once(':').unwrap();
        let expr = expr.trim();
        let words = expr.split(' ').collect::<Vec<_>>();

        if words.len() == 1 {
            numbers.insert(
                name,
                MonkeyNumber::Resolved(isize::from_str(words[0]).unwrap()),
            );
        } else {
            assert!(words.len() == 3);
            numbers.insert(
                name,
                MonkeyNumber::Unresolved(
                    words[0].to_string(),
                    Operation::parse(words[1].as_bytes()[0] as char),
                    words[2].to_string(),
                ),
            );
        }
    }

    while !numbers["root"].is_resolved() {
        for (name, number) in numbers.clone().iter() {
            match number {
                MonkeyNumber::Unresolved(left, op, right) => {
                    if let MonkeyNumber::Resolved(left_num) = numbers[left.as_str()] {
                        if let MonkeyNumber::Resolved(right_num) = numbers[right.as_str()] {
                            numbers.insert(
                                name,
                                MonkeyNumber::Resolved(match op {
                                    Operation::Add => left_num + right_num,
                                    Operation::Subtract => left_num - right_num,
                                    Operation::Multiply => left_num * right_num,
                                    Operation::Divide => left_num / right_num,
                                }),
                            );
                        }
                    }
                }
                _ => (),
            }
        }
    }

    println!("The root monkey yells: {:?}", numbers["root"]);
}

fn part2(contents: &mut String) {
    let mut numbers = HashMap::<&str, MonkeyNumber>::new();
    for line in contents.lines() {
        let (name, expr) = line.split_once(':').unwrap();
        let expr = expr.trim();
        let words = expr.split(' ').collect::<Vec<_>>();

        if words.len() == 1 {
            numbers.insert(
                name,
                MonkeyNumber::Resolved(isize::from_str(words[0]).unwrap()),
            );
        } else {
            assert!(words.len() == 3);
            numbers.insert(
                name,
                MonkeyNumber::Unresolved(
                    words[0].to_string(),
                    Operation::parse(words[1].as_bytes()[0] as char),
                    words[2].to_string(),
                ),
            );
        }
    }

    numbers.insert(
        "humn",
        MonkeyNumber::Unresolved("".to_string(), Operation::Add, "".to_string()),
    );

    while !numbers["root"].has_child_resolved(&numbers) {
        for (name, number) in numbers.clone().iter() {
            if name == &"humn" {
                continue;
            }
            match number {
                MonkeyNumber::Unresolved(left, op, right) => {
                    if let MonkeyNumber::Resolved(left_num) = numbers[left.as_str()] {
                        if let MonkeyNumber::Resolved(right_num) = numbers[right.as_str()] {
                            numbers.insert(
                                name,
                                MonkeyNumber::Resolved(match op {
                                    Operation::Add => left_num + right_num,
                                    Operation::Subtract => left_num - right_num,
                                    Operation::Multiply => left_num * right_num,
                                    Operation::Divide => left_num / right_num,
                                }),
                            );
                        }
                    }
                }
                _ => (),
            }
        }
    }

    let mut target = MonkeyNumber::Resolved(0);
    let mut result_name = "";
    let mut other_name = "";

    if let MonkeyNumber::Unresolved(left, _, right) = &numbers["root"] {
        let left_num = &numbers[left.as_str()];
        let right_num = &numbers[right.as_str()];

        assert!(left_num.is_resolved() || right_num.is_resolved());

        if left_num.is_resolved() {
            target = left_num.clone();
            result_name = left;
            other_name = right;
        } else if right_num.is_resolved() {
            target = right_num.clone();
            result_name = right;
            other_name = left;
        }
    }

    let mut current = &numbers[other_name];
    if current.left_resolved(&numbers) {
        // yes
    }

    println!("The monkey: {} has resolved to {:?}", result_name, target);

    println!("The root monkey yells: {:?}", numbers["root"]);
}

fn main() -> std::io::Result<()> {
    let mut f = File::open("input")?;
    let mut contents = String::new();
    f.read_to_string(&mut contents)?;

    //part1(&mut contents);
    part2(&mut contents);

    Ok(())
}
