use std::collections::HashMap;
use std::str::FromStr;
use std::{fs::File, io::Read};

fn part1(dirs: &HashMap<String, usize>) {
    let mut total = 0;

    for size in dirs.values() {
        if *size <= 100_000 {
            total += size;
        }
    }

    println!("{}", total);
}

fn part2(dirs: &HashMap<String, usize>) {
    let mut sizes = dirs.values().collect::<Vec<_>>();
    sizes.sort();

    let total_size = 70_000_000;
    let target_size = 30_000_000;

    let root_size = dirs["/"];
    let unused_space = total_size - root_size;

    for size in sizes {
        if unused_space + size >= target_size {
            println!("{}", size);
            break;
        }
    }
}

fn main() -> std::io::Result<()> {
    let mut f = File::open("input")?;
    let mut contents = String::new();
    f.read_to_string(&mut contents)?;
    contents = contents[0..contents.len() - 1].to_string();

    // current_path is the path starting from the root
    let mut current_path = vec!["/".to_string()];

    let mut dirs = HashMap::<String, usize>::new();
    dirs.insert("/".to_string(), 0);

    for (index, line) in contents.lines().enumerate() {
        if line.starts_with("$ cd") {
            let arg = line.split(" ").last().unwrap();
            match arg {
                ".." => {
                    current_path.pop();
                }
                "/" => {
                    current_path.clear();
                    current_path.push("/".to_string());
                }
                dir_name => {
                    current_path.push(dir_name.to_string());

                    if !dirs.contains_key(&current_path.join("")) {
                        dirs.insert(current_path.join(""), 0);
                    }
                }
            }
        } else if line.starts_with("$ ls") {
            for listing in contents.lines().skip(index + 1) {
                if listing.starts_with("$") {
                    break;
                }

                let (file_arg, _) = listing.split_once(" ").unwrap();

                if file_arg == "dir" {
                    continue;
                }

                let file_size = usize::from_str(file_arg).unwrap();

                let mut path = String::new();
                for dir in &current_path {
                    path += dir;
                    *dirs.get_mut(&path).unwrap() += file_size;
                }
            }
        }
    }

    part1(&dirs);
    part2(&dirs);

    Ok(())
}
