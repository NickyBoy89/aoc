use std::collections::BinaryHeap;
use std::str::FromStr;
use std::{fs::File, io::Read};

#[derive(Eq, PartialEq, Debug, Clone, Copy)]
enum Instr {
    Add(isize),
    Noop,
}

impl Instr {
    fn parse(input: &str) -> Instr {
        match &input[0..4] {
            "noop" => return Instr::Noop,
            "addx" => {
                return Instr::Add(isize::from_str(input.rsplit_once(" ").unwrap().1).unwrap())
            }
            _ => panic!("Unknown instruction"),
        }
    }
}

#[derive(Eq, Debug)]
struct PipelinedInstr {
    deadline: usize,
    instr: Instr,
}

impl PipelinedInstr {
    fn new(instr: Instr, deadline: usize) -> PipelinedInstr {
        PipelinedInstr { deadline, instr }
    }
}

impl Ord for PipelinedInstr {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        other.deadline.cmp(&self.deadline)
    }
}

impl PartialOrd for PipelinedInstr {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for PipelinedInstr {
    fn eq(&self, other: &Self) -> bool {
        self.deadline == other.deadline
    }
}

fn part1(instrs: &Vec<Instr>) {
    let mut pipeline = BinaryHeap::<PipelinedInstr>::new();
    let mut clock = 0;

    let mut register: isize = 1;

    let mut instrs = instrs.iter().peekable();

    let mut total_strength = 0;

    while instrs.peek() != None || pipeline.len() > 0 {
        clock += 1;

        if pipeline.len() == 0 {
            match instrs.next() {
                Some(instr) => match instr {
                    Instr::Add(_) => pipeline.push(PipelinedInstr::new(*instr, clock + 1)),
                    Instr::Noop => pipeline.push(PipelinedInstr::new(*instr, clock)),
                },
                None => {}
            }
        }

        if clock % 40 == 20 && clock <= 220 {
            total_strength += clock as isize * register;
        }

        while pipeline.len() > 0 && pipeline.peek().unwrap().deadline == clock {
            let finished = pipeline.pop().unwrap();
            match finished.instr {
                Instr::Add(n) => register += n,
                _ => {}
            }
        }
    }

    println!("Total strength: {}", total_strength);
}

#[derive(Clone, Copy)]
enum Pixel {
    Lit,
    Dark,
}

struct Screen {
    grid: [[Pixel; 40]; 6],
}

impl Screen {
    fn new() -> Screen {
        Screen {
            grid: [[Pixel::Dark; 40]; 6],
        }
    }

    fn display(self) {
        for row in self.grid {
            for col in row {
                match col {
                    Pixel::Lit => print!("#"),
                    Pixel::Dark => print!("."),
                }
            }
            print!("\n");
        }
    }

    fn set_pixel(&mut self, row: usize, column: usize, pixel_value: Pixel) {
        self.grid[row][column] = pixel_value;
    }
}

fn part2(instrs: &Vec<Instr>) {
    let mut pipeline = BinaryHeap::<PipelinedInstr>::new();
    let mut clock = 0;

    let mut register: isize = 1;

    let mut instrs = instrs.iter().peekable();

    let mut screen = Screen::new();

    while instrs.peek() != None || pipeline.len() > 0 {
        clock += 1;

        if pipeline.len() == 0 {
            match instrs.next() {
                Some(instr) => match instr {
                    Instr::Add(_) => pipeline.push(PipelinedInstr::new(*instr, clock + 1)),
                    Instr::Noop => pipeline.push(PipelinedInstr::new(*instr, clock)),
                },
                None => {}
            }
        }

        let current_row = (clock - 1) / 40;
        let current_col = (clock - 1) % 40;

        if (register - current_col as isize).abs() < 2 {
            screen.set_pixel(current_row, current_col, Pixel::Lit);
        }

        while pipeline.len() > 0 && pipeline.peek().unwrap().deadline == clock {
            match pipeline.pop().unwrap().instr {
                Instr::Add(n) => register += n,
                _ => {}
            }
        }
    }

    screen.display();
}

fn main() -> std::io::Result<()> {
    let mut f = File::open("input")?;
    let mut contents = String::new();
    f.read_to_string(&mut contents)?;

    let instrs = contents
        .lines()
        .map(|line| Instr::parse(line))
        .collect::<Vec<_>>();

    part1(&instrs);
    part2(&instrs);

    Ok(())
}
