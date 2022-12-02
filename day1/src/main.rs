use std::str::FromStr;
use std::{fs::File, io::Read};

fn main() -> std::io::Result<()> {
    let mut f = File::open("input")?;
    let mut contents = String::new();
    f.read_to_string(&mut contents)?;

    let mut calories = vec![0];

    for line in contents.split("\n") {
        let cal_size = calories.len() - 1;
        if line.len() > 0 {
            let n = usize::from_str(line).unwrap();
            calories[cal_size] += n;
        } else {
            calories.push(0);
        }
    }

    let mut max = 0;
    for n in &calories {
        if *n > max {
            max = *n;
        }
    }

    println!("The max is: {}", max);

    calories.sort();

    println!(
        "The elves are carrying {} calories in total",
        calories[calories.len() - 1] + calories[calories.len() - 2] + calories[calories.len() - 3]
    );

    Ok(())
}
