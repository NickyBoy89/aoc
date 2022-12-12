mod monkey;

use monkey::{Monkey, Operator};
use num_bigint::BigUint;
use std::{fs::File, io::Read};

#[derive(Clone, Debug)]
struct Throw {
    to: usize,
    worry_level: BigUint,
}

fn part1(monkeys: &mut Vec<Monkey>, _multiple: usize) {
    let mut pending_throws = Vec::<Throw>::new();

    for _ in 0..20 {
        for mut monkey in monkeys.iter_mut() {
            pending_throws.retain(|throw| {
                if throw.to == monkey.number {
                    monkey.starting_items.push(throw.worry_level.clone());
                    return false;
                }
                true
            });

            for worry_level in &monkey.starting_items {
                monkey.num_inspected += 1;

                let mut new_worry_level = if monkey.square_input {
                    worry_level.pow(2).clone()
                } else {
                    let input = BigUint::from(monkey.input);

                    match monkey.operator {
                        Operator::Add => worry_level + input,
                        Operator::Multiply => worry_level * input,
                    }
                };

                new_worry_level /= BigUint::from(3 as usize);

                pending_throws.push(Throw {
                    to: if &new_worry_level % &monkey.divisible_by == BigUint::from(0 as usize) {
                        monkey.throw_true
                    } else {
                        monkey.throw_false
                    },
                    worry_level: new_worry_level.clone(),
                });
            }

            monkey.starting_items.clear();
        }
    }

    let mut monkey_business = monkeys.iter().map(|m| m.num_inspected).collect::<Vec<_>>();
    monkey_business.sort_by(|a, b| b.partial_cmp(a).unwrap());
    println!(
        "Monkey business: {}",
        monkey_business[0] * monkey_business[1]
    );
}

fn part2(monkeys: &mut Vec<Monkey>, multiple: usize) {
    let mut pending_throws = Vec::<Throw>::new();

    for _ in 0..10_000 {
        for mut monkey in monkeys.iter_mut() {
            pending_throws.retain(|throw| {
                if throw.to == monkey.number {
                    monkey.starting_items.push(throw.worry_level.clone());
                    return false;
                }
                true
            });

            for worry_level in &monkey.starting_items {
                monkey.num_inspected += 1;

                let mut new_worry_level = if monkey.square_input {
                    worry_level.pow(2).clone()
                } else {
                    let input = BigUint::from(monkey.input);

                    match monkey.operator {
                        Operator::Add => worry_level + input,
                        Operator::Multiply => worry_level * input,
                    }
                };

                new_worry_level %= multiple;

                pending_throws.push(Throw {
                    to: if &new_worry_level % &monkey.divisible_by == BigUint::from(0 as usize) {
                        monkey.throw_true
                    } else {
                        monkey.throw_false
                    },
                    worry_level: new_worry_level.clone(),
                });
            }

            monkey.starting_items.clear();
        }
    }

    let mut monkey_business = monkeys.iter().map(|m| m.num_inspected).collect::<Vec<_>>();
    monkey_business.sort_by(|a, b| b.partial_cmp(a).unwrap());
    println!(
        "Monkey business: {}",
        monkey_business[0] * monkey_business[1]
    );
}

fn main() -> std::io::Result<()> {
    let mut f = File::open("input")?;
    let mut contents = String::new();
    f.read_to_string(&mut contents)?;

    let monkeys = contents
        .split("\n\n")
        .map(Monkey::parse)
        .collect::<Vec<_>>();

    let checks = monkeys.iter().map(|m| m.divisible_by).collect::<Vec<_>>();

    let mut multiple = 1;
    for check in checks {
        multiple *= check;
    }

    part1(&mut monkeys.clone(), multiple);
    part2(&mut monkeys.clone(), multiple);

    Ok(())
}
