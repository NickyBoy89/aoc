use num_bigint::BigUint;
use std::fmt;
use std::str::FromStr;
use std::{fs::File, io::Read};

#[derive(Debug)]
enum OperationInput {
    Old,
    Number(usize),
}

impl OperationInput {
    fn parse(input: &str) -> OperationInput {
        match input {
            "old" => OperationInput::Old,
            n => OperationInput::Number(usize::from_str(n).unwrap()),
        }
    }
}

#[derive(Debug)]
enum Operator {
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

#[derive(Debug)]
struct Monkey {
    number: usize,
    starting_items: Vec<Worry>,
    first_input: OperationInput,
    second_input: OperationInput,
    operator: Operator,
    divisible_by: usize,
    throw_true: usize,
    throw_false: usize,
    num_inspected: usize,
}

impl Monkey {
    fn parse(input: &str) -> Monkey {
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
            .map(|item| Worry::from(usize::from_str(item).unwrap()))
            .collect::<Vec<_>>();

        let operations = lines[2]["  Operation: new = ".len()..]
            .split(" ")
            .collect::<Vec<_>>();

        let first_input = OperationInput::parse(operations[0]);
        let operator = Operator::parse(operations[1]);
        let second_input = OperationInput::parse(operations[2]);

        let divisible_by = usize::from_str(&lines[3]["  Test: divisible by ".len()..]).unwrap();

        let throw_true =
            usize::from_str(&lines[4]["    If true: throw to monkey ".len()..]).unwrap();
        let throw_false =
            usize::from_str(&lines[5]["    If false: throw to monkey ".len()..]).unwrap();

        Monkey {
            number,
            starting_items,
            first_input,
            second_input,
            operator,
            divisible_by,
            throw_true,
            throw_false,
            num_inspected: 0,
        }
    }
}

#[derive(Clone, Debug)]
struct Worry {
    factors: Vec<usize>,
}

impl fmt::Display for Worry {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.factors)
    }
}

fn factor(n: usize) -> Vec<usize> {
    let mut num = n;
    let mut factors = Vec::new();

    let mut index = 2;
    while index <= num {
        if num % index == 0 {
            factors.push(index);
            num /= index;
            index = 2;
        } else {
            index += 1;
        }
    }

    factors
}

fn factor_biguint(n: BigUint) -> Vec<usize> {
    let mut num = n.clone();
    let mut factors = Vec::new();

    let zero = BigUint::from(0 as usize);

    let mut index = BigUint::from(2 as usize);
    while index.clone() <= num {
        if num.clone() % index.clone() == zero {
            factors.push(
                index
                    .iter_u64_digits()
                    .map(|x| x as usize)
                    .collect::<Vec<usize>>()[0],
            );
            num /= index.clone();
        } else {
            index += 1 as usize;
        }
    }

    factors
}

impl Worry {
    fn from(n: usize) -> Worry {
        Worry { factors: factor(n) }
    }

    fn total(&self) -> BigUint {
        let mut total = BigUint::from(1 as usize);
        for factor in &self.factors {
            total *= *factor;
        }
        total
    }

    fn add(&self, n: Worry) -> Worry {
        Worry {
            factors: factor_biguint(self.total() + n.total()),
        }
    }

    fn mul(&self, n: Worry) -> Worry {
        let mut new = Worry {
            factors: self.factors.clone(),
        };

        for factor in n.factors {
            new.factors.push(factor);
        }

        new
    }

    fn div_by(&self, n: usize) -> Worry {
        let mut new = Worry {
            factors: self.factors.clone(),
        };

        match new.factors.iter().position(|f| *f == n) {
            None => new.factors = factor_biguint(self.total() / n),
            Some(index) => {
                new.factors.remove(index);
            }
        }

        new
    }

    fn is_factor(self, n: usize) -> bool {
        self.factors.contains(&n)
    }
}

#[derive(Clone)]
struct Throw {
    to: usize,
    worry_level: Worry,
}

fn part1(contents: &mut String) {
    let mut monkeys = contents
        .split("\n\n")
        .map(Monkey::parse)
        .collect::<Vec<_>>();

    let mut pending_throws = Vec::<Throw>::new();

    for _round in 0..10_000 {
        for mut monkey in monkeys.iter_mut() {
            //println!("Monkey {}", monkey.number);
            pending_throws.retain(|throw| {
                if throw.to == monkey.number {
                    monkey.starting_items.push(throw.worry_level.clone());
                    return false;
                }
                true
            });

            for worry_level in &monkey.starting_items {
                monkey.num_inspected += 1;
                //println!("Inspecting item with worry {}", worry_level);
                let first = match monkey.first_input {
                    OperationInput::Old => worry_level.clone(),
                    OperationInput::Number(n) => Worry::from(n),
                };

                let second = match monkey.second_input {
                    OperationInput::Old => worry_level.clone(),
                    OperationInput::Number(n) => Worry::from(n),
                };

                let mut new_worry_level = match monkey.operator {
                    Operator::Add => first.add(second),
                    Operator::Multiply => first.mul(second),
                };

                //println!("Worry is now {}", new_worry_level);

                new_worry_level = new_worry_level.div_by(3);

                //println!("Divided by 3 to {}", new_worry_level);

                pending_throws.push(Throw {
                    to: if new_worry_level.clone().is_factor(monkey.divisible_by) {
                        //println!("Divisible by {}", monkey.divisible_by);
                        monkey.throw_true
                    } else {
                        //println!("Not divisible by {}", monkey.divisible_by);
                        monkey.throw_false
                    },
                    worry_level: new_worry_level.clone(),
                });

                /*
                println!(
                    "Item with worry {} is thrown to monkey {}",
                    new_worry_level.clone(),
                    if new_worry_level.is_factor(monkey.divisible_by) {
                        monkey.throw_true
                    } else {
                        monkey.throw_false
                    }
                );
                */
            }

            monkey.starting_items.clear();
        }
        println!("Finished round {}", _round);
    }

    let mut monkey_business = monkeys.iter().map(|m| m.num_inspected).collect::<Vec<_>>();
    monkey_business.sort_by(|a, b| b.partial_cmp(a).unwrap());
    println!("{:?}", monkey_business);
    println!(
        "Monkey business: {}",
        monkey_business[0] * monkey_business[1]
    );

    println!("{:?}", monkeys);
}

fn part2(contents: &mut String) {}

fn main() -> std::io::Result<()> {
    let mut f = File::open("input")?;
    let mut contents = String::new();
    f.read_to_string(&mut contents)?;

    part1(&mut contents);
    //part2(&mut contents);

    Ok(())
}
