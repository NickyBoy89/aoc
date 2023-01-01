use std::collections::{HashMap, VecDeque};
use std::str::FromStr;
use std::{fs::File, io::Read};

#[derive(Debug)]
struct Snafu {
    number: isize,
}

impl Snafu {
    fn as_snafu(&self) -> String {
        let mut max_digit = 0;
        while self.number > 5isize.pow(max_digit) {
            max_digit += 1;
        }

        let mut digits = VecDeque::new();

        let mut altered = self.number;
        for digit in 0..max_digit - 1 {
            let factor = 5isize.pow(digit);
            let next_factor = 5isize.pow(digit + 1);
            for shift in -2..=2 {
                if (altered + (factor * shift)) % next_factor == 0 {
                    altered += factor * shift;
                    digits.push_front(shift);
                    break;
                }
            }
        }

        let factor = 5isize.pow(max_digit - 1);
        for shift in -2..=2 {
            if (altered + (factor * shift)) == 0 {
                altered += factor * shift;
                digits.push_front(shift);
                break;
            }
        }

        // I'm not sure why, but inverting all the digits works
        digits = digits.iter().map(|digit| digit * -1).collect::<VecDeque<isize>>();

        let mut result = String::new();
        for digit in digits {
            result += match digit {
                -2 => "=",
                -1 => "-",
                0 => "0",
                1 => "1",
                2 => "2",
                other => panic!("Unknown shift amount: {}", other),
            }
        }

        assert_eq!(self.number, Snafu::from_str(result.as_str()).number);

        result
    }

    fn from_str(input: &str) -> Snafu {
        let mut result = 0;

        let digits = input.chars().count() - 1;

        for (ind, c) in input.chars().enumerate() {
            let place = 5isize.pow((digits - ind) as u32);
            match c {
                '2' => result += 2 * place,
                '1' => result += 1 * place,
                '0' => result += 0 * place,
                '-' => result += -1 * place,
                '=' => result += -2 * place,
                digit => panic!("Unknown digit: {}", digit),
            }
        }
        Snafu { number: result }
    }
}

impl From<isize> for Snafu {
    fn from(number: isize) -> Self {
        Snafu { number }
    }
}

fn part1(contents: &mut String) {
    let numbers = contents.lines().map(Snafu::from_str).collect::<Vec<_>>();

    let sum = numbers.iter().map(|n| n.number).sum::<isize>();

    println!("Sum is: {}", Snafu::from(sum).as_snafu());
}

fn part2(contents: &mut String) {}

fn main() -> std::io::Result<()> {
    let mut f = File::open("input")?;
    let mut contents = String::new();
    f.read_to_string(&mut contents)?;
    part1(&mut contents);
    part2(&mut contents);

    Ok(())
}
