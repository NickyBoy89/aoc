#![feature(linked_list_remove)]

use std::collections::{HashMap, LinkedList, VecDeque};
use std::fmt::{write, Display};
use std::str::FromStr;
use std::{fs::File, io::Read};

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
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
    numbers: LinkedList<Number>,
}

impl EncryptedFile {
    fn move_at(&mut self, index: usize) {
        let shift = self.numbers.iter().nth(index).unwrap();

        let new_ind = index as isize + shift.value;
        let mut actual_ind = new_ind.rem_euclid(self.numbers.len() as isize) as usize;

        if new_ind > 0 {
            actual_ind = (actual_ind + 1).rem_euclid(self.numbers.len());
        } else if shift.value < 0 && actual_ind == 0 {
            actual_ind = (actual_ind as isize - 1).rem_euclid(self.numbers.len() as isize) as usize;
        }

        println!("Actual index is {}", actual_ind);

        let mut new_numbers = LinkedList::<Number>::new();
        for (ind, number) in self.numbers.iter().enumerate() {
            if ind == actual_ind {
                new_numbers.push_back(shift.clone());
            }
            new_numbers.push_back(number.clone());
        }

        self.numbers = new_numbers
            .into_iter()
            .enumerate()
            .filter(|(ind, number)| number != shift || (number == shift && *ind == actual_ind))
            .map(|(_, number)| number)
            .collect();
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
        .collect::<LinkedList<_>>();

    let mut file = EncryptedFile { numbers };
    let mut visited = vec![false; file.numbers.len()];
    let mut num_visited = 0;

    let total_numbers = file.numbers.len();

    while num_visited < total_numbers {
        for (ind, number) in file.numbers.iter().enumerate() {
            if !visited[number.original_index] {
                visited[number.original_index] = true;
                num_visited += 1;

                println!("Before:");
                file.display();
                println!("Moving number {} at index {}", number, ind);
                file.move_at(ind);
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

    /*
    println!(
        "The sum of coordinates is: {}",
        coordinates
            .iter()
            .map(|number| file.numbers[(number + zero_ind) % file.numbers.len()].value)
            .sum::<isize>()
    )
        */
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
