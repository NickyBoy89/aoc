use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();

    let mut first_list = Vec::new();
    let mut second_list = Vec::new();

    for line in input.lines() {
        let (fst, snd) = line.split_once("   ").unwrap();
        first_list.push(fst.parse::<isize>().unwrap());
        second_list.push(snd.parse::<isize>().unwrap());
    }

    first_list.sort();
    second_list.sort();

    println!(
        "{:?}",
        first_list
            .iter()
            .zip(&second_list)
            .map(|(x, y)| (x - y).abs())
            .sum::<isize>()
    );

    println!(
        "{:?}",
        first_list
            .iter()
            .map(|x| x * second_list.iter().filter(|&item| item == x).count() as isize)
            .sum::<isize>()
    );
}
