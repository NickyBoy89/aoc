use std::fs;

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
enum LevelOrder {
    Increasing,
    Decreasing,
}

fn check_diff(digits: &Vec<isize>) -> bool {
    let diffs = digits
        .as_slice()
        .windows(2)
        .map(|arr| arr[0] - arr[1])
        .collect::<Vec<_>>();

    let mut ordering = None;

    for &diff in diffs.iter() {
        if diff.abs() < 1 || diff.abs() > 3 {
            return false;
        }

        if ordering.is_none() {
            if diff > 0 {
                ordering = Some(LevelOrder::Decreasing);
            } else {
                ordering = Some(LevelOrder::Increasing);
            }
        } else {
            if diff > 0 {
                if ordering.unwrap() == LevelOrder::Increasing {
                    return false;
                }
            } else {
                if ordering.unwrap() == LevelOrder::Decreasing {
                    return false;
                }
            }
        }
    }

    true
}

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();

    let mut num_safe = 0;
    let mut num_safe_part1 = 0;

    for line in input.lines() {
        let digits = line
            .split(" ")
            .map(|x| x.parse::<isize>().unwrap())
            .collect::<Vec<_>>();

        if check_diff(&digits) {
            num_safe_part1 += 1;
        }

        let mut test_digits = (0..digits.len())
            .map(|ind| {
                let mut new_arr = digits.clone();
                new_arr.remove(ind);
                new_arr
            })
            .collect::<Vec<Vec<_>>>();
        test_digits.push(digits);

        let results = test_digits
            .iter()
            .map(|dig| check_diff(dig))
            .collect::<Vec<_>>();

        if results.iter().any(|&r| r) {
            num_safe += 1;
        }
    }

    println!("Num safe (part 1): {:?}", num_safe_part1);
    println!("Num safe: {:?}", num_safe);
}
