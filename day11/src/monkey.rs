use num_bigint::BigUint;

use std::str::FromStr;

#[derive(Debug, Clone, Copy)]
pub enum Operator {
    Add,
    Multiply,
}

impl Operator {
    fn parse(input: &str) -> Operator {
        match input.as_bytes()[0] as char {
            '+' => Operator::Add,
            '*' => Operator::Multiply,
            other => panic!("Unknown operator {}", other),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Monkey {
    pub number: usize,
    pub starting_items: Vec<BigUint>,
    pub square_input: bool,
    pub input: usize,
    pub operator: Operator,
    pub divisible_by: usize,
    pub throw_true: usize,
    pub throw_false: usize,
    pub num_inspected: usize,
}

impl Monkey {
    pub fn parse(input: &str) -> Monkey {
        let lines = input.lines().collect::<Vec<_>>();

        let number = usize::from_str(
            lines[0]
                .split_once(" ")
                .unwrap()
                .1
                .strip_suffix(":")
                .unwrap(),
        )
        .unwrap();

        let starting_items = lines[1]["  Starting items: ".len()..]
            .split(", ")
            .map(|item| BigUint::from(usize::from_str(item).unwrap()))
            .collect::<Vec<_>>();

        let operations = lines[2]["  Operation: new = ".len()..]
            .split(" ")
            .collect::<Vec<_>>();

        let square_input = operations[0] == "old" && operations[2] == "old";

        let operator = Operator::parse(operations[1]);
        let input = usize::from_str(operations[2]).unwrap_or(0);

        let divisible_by = usize::from_str(&lines[3]["  Test: divisible by ".len()..]).unwrap();

        let throw_true =
            usize::from_str(&lines[4]["    If true: throw to monkey ".len()..]).unwrap();
        let throw_false =
            usize::from_str(&lines[5]["    If false: throw to monkey ".len()..]).unwrap();

        Monkey {
            number,
            starting_items,
            square_input,
            input,
            operator,
            divisible_by,
            throw_true,
            throw_false,
            num_inspected: 0,
        }
    }
}
