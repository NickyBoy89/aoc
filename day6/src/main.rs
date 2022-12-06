use std::{fs::File, io::Read};

fn part1(contents: &mut String) {
    let mut counter = 0;

    for window in contents.chars().collect::<Vec<_>>().windows(4) {
        let mut valid = true;
        for (ind, item) in window.iter().enumerate() {
            for (found_ind, found_item) in window.iter().enumerate() {
                if item == found_item && found_ind != ind {
                    valid = false;
                    break;
                }
            }
        }

        if valid {
            println!("Counter: {}", counter + 4);
            return;
        }

        counter += 1;
    }

    panic!("Did not find a sequence");
}

fn part2(contents: &mut String) {
    let mut counter = 0;

    let window_size = 14;

    for window in contents.chars().collect::<Vec<_>>().windows(window_size) {
        let mut valid = true;
        for (ind, item) in window.iter().enumerate() {
            for (found_ind, found_item) in window.iter().enumerate() {
                if item == found_item && found_ind != ind {
                    valid = false;
                    break;
                }
            }
        }

        if valid {
            println!("Counter: {}", counter + window_size);
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
