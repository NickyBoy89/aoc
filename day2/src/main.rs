#![feature(str_split_as_str)]
use std::{fs::File, io::Read};

fn score_win() -> usize {
    return 6;
}

fn score_tie() -> usize {
    return 3;
}

fn score_rock() -> usize {
    return 1;
}

fn score_paper() -> usize {
    return 2;
}

fn score_scissors() -> usize {
    return 3;
}

fn part1(contents: &mut String) -> usize {
    let mut score = 0;
    for line in contents.split("\n") {
        let mut matches = line.split(" ");

        match matches.next().unwrap() {
            "A" => match matches.next().unwrap() {
                // Rock
                "X" => {
                    // Rock
                    score += score_tie();
                    score += score_rock();
                }
                "Y" => {
                    // Paper
                    score += score_win();
                    score += score_paper();
                }
                "Z" => {
                    // Scissors
                    score += score_scissors();
                }
                _ => unreachable!(),
            },
            "B" => match matches.next().unwrap() {
                // Paper
                "X" => {
                    // Rock
                    score += score_rock();
                }
                "Y" => {
                    // Paper
                    score += score_tie();
                    score += score_paper();
                }
                "Z" => {
                    // Scissors
                    score += score_win();
                    score += score_scissors();
                }
                _ => unreachable!(),
            },

            "C" => match matches.next().unwrap() {
                // Scissors
                "X" => {
                    // Rock
                    score += score_win();
                    score += score_rock();
                }
                "Y" => {
                    // Paper
                    score += score_paper();
                }
                "Z" => {
                    // Scissors
                    score += score_tie();
                    score += score_scissors();
                }
                _ => unreachable!(),
            },

            _ => continue,
        }
    }

    score
}

fn part2(contents: &mut String) -> usize {
    let mut score = 0;
    for line in contents.split("\n") {
        let mut matches = line.split(" ");

        match matches.next().unwrap() {
            "A" => match matches.next().unwrap() {
                // Rock
                "X" => {
                    // Lose
                    score += score_scissors();
                }
                "Y" => {
                    // Tie
                    score += score_tie();
                    score += score_rock();
                }
                "Z" => {
                    // Win
                    score += score_win();
                    score += score_paper();
                }
                _ => unreachable!(),
            },
            "B" => match matches.next().unwrap() {
                // Paper
                "X" => {
                    // Lose
                    score += score_rock();
                }
                "Y" => {
                    // Tie
                    score += score_tie();
                    score += score_paper();
                }
                "Z" => {
                    // Win
                    score += score_win();
                    score += score_scissors();
                }
                _ => unreachable!(),
            },

            "C" => match matches.next().unwrap() {
                // Scissors
                "X" => {
                    // Lose
                    score += score_paper();
                }
                "Y" => {
                    // Tie
                    score += score_tie();
                    score += score_scissors();
                }
                "Z" => {
                    // Win
                    score += score_win();
                    score += score_rock();
                }
                _ => unreachable!(),
            },

            _ => continue,
        }
    }

    score
}

fn main() -> std::io::Result<()> {
    let mut f = File::open("input")?;
    let mut contents = String::new();
    f.read_to_string(&mut contents)?;

    println!("{}", part1(&mut contents));
    println!("{}", part2(&mut contents));

    Ok(())
}
