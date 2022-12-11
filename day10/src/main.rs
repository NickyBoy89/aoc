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
            "noop" => Instr::Noop,
            "addx" => Instr::Add(isize::from_str(input.rsplit_once(" ").unwrap().1).unwrap()),
            _ => panic!("Unknown instruction"),
        }
    }
}

#[derive(Debug)]
struct PipelinedInstr {
    deadline: usize,
    instr: Instr,
}

impl PipelinedInstr {
    fn new(instr: Instr, deadline: usize) -> PipelinedInstr {
        PipelinedInstr { deadline, instr }
    }
}

fn part1(instrs: &[Instr]) {
    let mut clock = 0;

    let mut executing: Option<PipelinedInstr> = None;
    let mut register: isize = 1;

    let mut instrs = instrs.iter().peekable();

    let mut total_strength = 0;

    while instrs.peek() != None || executing.is_some() {
        clock += 1;

        if executing.is_none() {
            if let Some(instr) = instrs.next() {
                match instr {
                    Instr::Add(_) => executing = Some(PipelinedInstr::new(*instr, clock + 1)),
                    Instr::Noop => executing = Some(PipelinedInstr::new(*instr, clock)),
                }
            }
        }

        if clock % 40 == 20 && clock <= 220 {
            total_strength += clock as isize * register;
        }

        if let Some(curr_instr) = &executing {
            if curr_instr.deadline == clock {
                if let Instr::Add(n) = curr_instr.instr {
                    register += n;
                }

                executing = None;
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
            println!();
        }
    }

    fn set_pixel(&mut self, row: usize, column: usize, pixel_value: Pixel) {
        self.grid[row][column] = pixel_value;
    }
}

fn part2(instrs: &[Instr]) {
    let mut clock = 0;

    let mut executing: Option<PipelinedInstr> = None;
    let mut register: isize = 1;

    let mut instrs = instrs.iter().peekable();

    let mut screen = Screen::new();

    while instrs.peek() != None || executing.is_some() {
        clock += 1;

        if executing.is_none() {
            if let Some(instr) = instrs.next() {
                match instr {
                    Instr::Add(_) => executing = Some(PipelinedInstr::new(*instr, clock + 1)),
                    Instr::Noop => executing = Some(PipelinedInstr::new(*instr, clock)),
                }
            }
        }

        let current_row = (clock - 1) / 40;
        let current_col = (clock - 1) % 40;

        if (register - current_col as isize).abs() < 2 {
            screen.set_pixel(current_row, current_col, Pixel::Lit);
        }

        if let Some(curr_instr) = &executing {
            if curr_instr.deadline == clock {
                if let Instr::Add(n) = curr_instr.instr {
                    register += n;
                }

                executing = None;
            }
        }
    }

    screen.display();
}

fn main() -> std::io::Result<()> {
    let mut f = File::open("input")?;
    let mut contents = String::new();
    f.read_to_string(&mut contents)?;

    let instrs = contents.lines().map(Instr::parse).collect::<Vec<_>>();

    part1(&instrs);
    part2(&instrs);

    Ok(())
}
