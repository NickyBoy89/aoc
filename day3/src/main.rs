use std::fs;
use std::str;

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
enum State {
    FirstNum,
    SecondNum,
    Finished,
    Invalid,
}

#[derive(Debug)]
struct Mul {
    x: usize,
    y: usize,
    enabled: bool,
}

#[derive(Debug)]
enum ControlFlow {
    Do(usize),
    Dont(usize),
}

impl ControlFlow {
    fn as_index(&self) -> usize {
        match self {
            ControlFlow::Do(ind) => *ind,
            ControlFlow::Dont(ind) => *ind,
        }
    }
}

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();

    let mut instrs = Vec::new();

    let mut flow = Vec::new();

    for (ind, group) in input.as_bytes().windows("don't()".len()).enumerate() {
        let group_str = str::from_utf8(group).unwrap();
        if group_str == "don't()" {
            flow.push(ControlFlow::Dont(ind));
        } else if group_str.starts_with("do()") {
            flow.push(ControlFlow::Do(ind));
        }
    }

    let mut current_index = 0;

    let mut enabled = true;

    for candidate in input.split("mul(") {
        while !flow.is_empty() && current_index >= flow[0].as_index() {
            println!("Control flow: {:?}", flow[0]);
            match flow[0] {
                ControlFlow::Do(_) => enabled = true,
                ControlFlow::Dont(_) => enabled = false,
            };
            flow.remove(0);
        }

        current_index += candidate.len() + "mul(".len();

        println!("Starting at index: {:?}", current_index);

        let mut first_num = String::new();
        let mut second_num = String::new();

        let mut state = State::FirstNum;

        for c in candidate.chars() {
            // println!("Parsing: {:?}", c);
            match c {
                '0'..='9' => match state {
                    State::FirstNum => first_num.push(c),
                    State::SecondNum => second_num.push(c),
                    _ => unreachable!(),
                },
                ',' => {
                    match state {
                        State::SecondNum => {
                            state = State::Invalid;
                            break;
                        }
                        State::FirstNum => {
                            if first_num.len() < 1 || first_num.len() > 3 {
                                state = State::Invalid;
                                break;
                            }
                            state = State::SecondNum;
                        }
                        _ => unreachable!(),
                    };
                }
                ')' => match state {
                    State::FirstNum | State::Finished => {
                        state = State::Invalid;
                        break;
                    }
                    State::SecondNum => {
                        if second_num.len() < 1 || second_num.len() > 3 {
                            state = State::Invalid;
                            break;
                        }
                        state = State::Finished;
                        break;
                    }
                    _ => unreachable!(),
                },
                _ => {
                    state = State::Invalid;
                    break;
                }
            }
        }

        if state == State::Invalid {
            println!("Match was invalid!, text was: {:?}", candidate);
        } else {
            assert_eq!(state, State::Finished);
            instrs.push(Mul {
                x: first_num.parse::<usize>().unwrap(),
                y: second_num.parse::<usize>().unwrap(),
                enabled,
            });
            println!(
                "Match group: mul({:?},{:?}), original: {:?}",
                first_num, second_num, candidate
            );
        }
    }

    let mut total = 0;

    for instr in instrs {
        if !instr.enabled {
            println!("Instr {:?} was disabled", instr);
        } else {
            total += instr.x * instr.y;
        }
    }

    println!("Total: {:?}", total);
}
