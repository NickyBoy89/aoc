use std::collections::VecDeque;
use std::str::FromStr;
use std::{fs::File, io::Read};

struct MoveCommand {
    number: usize,
    src: usize,
    dest: usize,
}

impl MoveCommand {
    fn parse(source: &str) -> MoveCommand {
        let parts: Vec<&str> = source.split(" ").collect();
        return MoveCommand {
            number: usize::from_str(parts[1]).unwrap(),
            src: usize::from_str(parts[3]).unwrap(),
            dest: usize::from_str(parts[5]).unwrap(),
        };
    }
}

fn part1(stacks: &mut Vec<VecDeque<char>>, moves: &Vec<MoveCommand>) {
    for command in moves {
        for _ in 0..command.number {
            let to_add = stacks[command.src - 1].pop_front().unwrap();
            stacks[command.dest - 1].push_front(to_add);
        }
    }

    let mut crates = String::new();

    for stack in stacks {
        crates.push(stack.pop_front().unwrap());
    }

    println!("{:?}", crates);
}

fn part2(stacks: &mut Vec<VecDeque<char>>, moves: &Vec<MoveCommand>) {
    for command in moves {
        let mut temp = vec![];
        for _ in 0..command.number {
            let to_add = stacks[command.src - 1].pop_front().unwrap();
            temp.push(to_add);
        }
        for item in temp.iter().rev() {
            stacks[command.dest - 1].push_front(*item);
        }
    }

    let mut crates = String::new();

    for stack in stacks {
        crates.push(stack.pop_front().unwrap());
    }

    println!("{:?}", crates);
}

fn main() -> std::io::Result<()> {
    let mut f = File::open("input")?;
    let mut contents = String::new();
    f.read_to_string(&mut contents)?;

    let line_size = contents.lines().next().unwrap().len();
    let num_containers = (line_size + 1) / 4;
    // stacks represents the stacks of crates. The outer vector is the stack, and the other is the
    // index of the box
    let mut stacks: Vec<VecDeque<char>> = vec![VecDeque::new(); num_containers];

    let mut block_end = 0;

    for (ind, line) in contents.lines().enumerate() {
        let mut letters = vec![];

        let mut done = false;

        let chars: Vec<char> = line.chars().collect();

        for (ind, c) in chars.iter().enumerate() {
            if ind == 1 && c.is_numeric() {
                done = true;
                break;
            }
            if ind % 4 == 1 {
                letters.push(c);
            }
        }

        for (ind, letter) in letters.into_iter().enumerate() {
            if *letter != ' ' {
                stacks[ind].push_back(*letter);
            }
        }

        if done {
            block_end = ind;
            break;
        }
    }

    let moves: Vec<MoveCommand> = contents
        .lines()
        .skip(block_end + 2)
        .map(|x| MoveCommand::parse(x))
        .collect();

    part1(&mut stacks.clone(), &moves);
    part2(&mut stacks.clone(), &moves);

    Ok(())
}
