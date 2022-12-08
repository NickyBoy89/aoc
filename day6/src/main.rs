use std::{collections::HashSet, fs::File, io::Read};

fn part1(contents: &mut String) {
    let mut counter = 0;

    const WINDOW_SIZE: usize = 4;

    for window in contents.as_bytes().windows(WINDOW_SIZE) {
        if HashSet::<u8>::from_iter(window.iter().copied()).len() == WINDOW_SIZE {
            println!("Counter: {}", counter + WINDOW_SIZE);
            return;
        }

        counter += 1;
    }

    panic!("Did not find a sequence");
}

fn part2(contents: &mut String) {
    let mut counter = 0;

    const WINDOW_SIZE: usize = 14;

    for window in contents.as_bytes().windows(WINDOW_SIZE) {
        if HashSet::<u8>::from_iter(window.iter().copied()).len() == WINDOW_SIZE {
            println!("Counter: {}", counter + WINDOW_SIZE);
            return;
        }

        counter += 1;
    }

    panic!("Did not find a sequence");
}

fn main() -> std::io::Result<()> {
    let mut f = File::open("input")?;
    let mut contents = String::new();
    f.read_to_string(&mut contents)?;

    part1(&mut contents);
    part2(&mut contents);

    Ok(())
}
