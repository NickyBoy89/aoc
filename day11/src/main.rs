mod monkey;
mod worry;

use monkey::{Monkey, OperationInput, Operator};
use std::{fs::File, io::Read};
use worry::Worry;

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

    let checks = monkeys.iter().map(|m| m.divisible_by).collect::<Vec<_>>();

    for monkey in &mut monkeys {
        for item in &mut monkey.starting_items {
            item.add_factors(&checks)
        }
    }

    for round in 0..10_000 {
        for mut monkey in monkeys.iter_mut() {
            println!("Monkey {}", monkey.number);
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
                    OperationInput::Number(n) => Worry::from_with_factors(n, &checks),
                };

                let second = match monkey.second_input {
                    OperationInput::Old => worry_level.clone(),
                    OperationInput::Number(n) => Worry::from_with_factors(n, &checks),
                };

                let mut new_worry_level = match monkey.operator {
                    Operator::Add => first.add(second),
                    Operator::Multiply => first.mul(second),
                };

                //println!("Worry is now {}", new_worry_level);

                //new_worry_level = new_worry_level.div_by(3);

                //println!("Divided by 3 to {}", new_worry_level);

                pending_throws.push(Throw {
                    to: if new_worry_level.clone().is_divisible_by(monkey.divisible_by) {
                        println!("Divisible by {}", monkey.divisible_by);
                        monkey.throw_true
                    } else {
                        println!("Not divisible by {}", monkey.divisible_by);
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
        println!("Finished round {}", round);
    }

    let mut monkey_business = monkeys.iter().map(|m| m.num_inspected).collect::<Vec<_>>();
    monkey_business.sort_by(|a, b| b.partial_cmp(a).unwrap());
    println!("Swaps: {:?}", monkey_business);
    println!(
        "Monkey business: {}",
        monkey_business[0] * monkey_business[1]
    );
}

fn part2(_contents: &mut String) {}

fn main() -> std::io::Result<()> {
    let mut f = File::open("input")?;
    let mut contents = String::new();
    f.read_to_string(&mut contents)?;

    part1(&mut contents);
    //part2(&mut contents);

    Ok(())
}
