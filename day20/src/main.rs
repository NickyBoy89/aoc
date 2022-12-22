use std::collections::{HashMap, VecDeque};
use std::fmt::{write, Display};
use std::str::FromStr;
use std::{fs::File, io::Read};

#[derive(Debug, Clone, Copy)]
struct Number {
    value: isize,
    original_index: usize,
}

impl Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug)]
struct EncryptedFile {
    numbers: Vec<Number>,
}

impl EncryptedFile {
    fn move_at(&mut self, index: usize) {
        let number = self.numbers[index];
        let mut temp = self.numbers.chunks(1).collect::<Vec<_>>();
        let new_ind = index as isize + number.value;
        let actual_ind = (new_ind).rem_euclid(self.numbers.len() as isize) as usize;

        if new_ind > 0 {
            for ind in index..actual_ind {
                temp.swap(ind, (ind + 1).rem_euclid(self.numbers.len()));
            }
        } else {
            for ind in (actual_ind..index).rev() {
                temp.swap(ind, (ind + 1).rem_euclid(self.numbers.len()));
            }
        }

        self.numbers = temp.into_iter().flatten().map(|n| *n).collect::<Vec<_>>();
    }

    fn display(&self) {
        print!("EncryptedFile {{ ");
        for num in &self.numbers {
            print!("{}, ", num.value);
        }
        println!("}}");
    }
}

fn part1(contents: &mut String) {
    let numbers = contents
        .lines()
        .enumerate()
        .map(|(index, line)| Number {
            value: isize::from_str(line).unwrap(),
            original_index: index,
        })
        .collect::<Vec<_>>();

    let mut file = EncryptedFile { numbers };
    let mut visited = vec![false; file.numbers.len()];
    let mut num_visited = 0;

    let total_numbers = file.numbers.len();

    while num_visited < total_numbers {
        for (ind, number) in file.numbers.iter().enumerate() {
            if !visited[number.original_index] {
                visited[number.original_index] = true;
                num_visited += 1;

                file.move_at(ind);
                println!("File:");
                file.display();
                break;
            }
        }
    }

    return;

    let zero_ind = file
        .numbers
        .iter()
        .enumerate()
        .find(|(_, number)| number.value == 0)
        .unwrap()
        .0;

    let coordinates = [1000, 2000, 3000];

    println!(
        "The sum of coordinates is: {}",
        coordinates
            .iter()
            .map(|number| file.numbers[(number + zero_ind) % file.numbers.len()].value)
            .sum::<isize>()
    )
}

//fn part2(contents: &mut String) {}

fn main() -> std::io::Result<()> {
    let mut f = File::open("input")?;
    let mut contents = String::new();
    f.read_to_string(&mut contents)?;

    part1(&mut contents);
    //part2(&mut contents);

    Ok(())
}
