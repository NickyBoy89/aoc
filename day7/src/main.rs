use itertools::Itertools;
use std::fs;

fn concat_nums(x: usize, y: usize) -> usize {
    format!("{}{}", x, y).parse::<usize>().unwrap()
}

fn eval_equation(equation: &Vec<usize>, ops: Vec<char>) -> usize {
    let mut total = equation[0];
    let mut equation = equation.clone();
    equation.remove(0);
    for (ind, n) in equation.iter().enumerate() {
        match ops[ind] {
            '+' => total += n,
            '*' => total *= n,
            '|' => total = concat_nums(total, *n),
            _ => unreachable!(),
        }
    }
    total
}

fn part1(equations: &Vec<(usize, Vec<usize>)>) {
    let mut total_calibration = 0;

    for (target, eq) in equations {
        let ops =
            itertools::repeat_n(vec!['+', '*'].into_iter(), eq.len() - 1).multi_cartesian_product();

        for operator_list in ops {
            if operator_list.len() != eq.len() - 1 {
                continue;
            }
            if eval_equation(&eq, operator_list) == *target {
                // println!("Equation {:?} can be valid", (target, &eq));
                total_calibration += target;
                break;
            }
        }
    }

    println!("Total calibration: {:?}", total_calibration);
}

fn part2(equations: &Vec<(usize, Vec<usize>)>) {
    let mut total_calibration = 0;

    for (target, eq) in equations {
        let ops = itertools::repeat_n(vec!['+', '*', '|'].into_iter(), eq.len() - 1)
            .multi_cartesian_product();

        for operator_list in ops {
            if operator_list.len() != eq.len() - 1 {
                continue;
            }
            if eval_equation(&eq, operator_list) == *target {
                // println!("Equation {:?} can be valid", (target, &eq));
                total_calibration += target;
                break;
            }
        }
    }

    println!("Total calibration: {:?}", total_calibration);
}

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();

    let equations = input
        .lines()
        .map(|line| {
            let (total, ns) = line.split_once(": ").unwrap();
            let ns = ns
                .split(" ")
                .map(|n| n.parse::<usize>().unwrap())
                .collect::<Vec<_>>();
            (total.parse::<usize>().unwrap(), ns)
        })
        .collect::<Vec<_>>();

    part1(&equations);
    part2(&equations);
}
