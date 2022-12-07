use std::str::FromStr;
use std::{fs::File, io::Read};

struct Directory<'a> {
    name: String,
    size: usize,
    parent: Option<&'a Directory<'a>>,
    files: Vec<&'a Directory<'a>>,
}

impl Directory<'_> {
    fn new(name: String, size: usize) -> Directory<'static> {
        return Directory {
            name,
            size: 0,
            parent: None,
            files: Vec::new(),
        };
    }

    fn find_file(self, name: String) -> Option<&'static Directory<'static>> {
        for file in self.files {
            if file.name == name {
                return Some(file);
            }
        }

        return None;
    }

    fn add_file(&mut self, name: String, size: usize) -> &Directory {
        let mut new_file = &Directory::new(name, size);
        self.files.push(new_file);
        return new_file;
    }

    fn directory_size(self) -> usize {
        if self.files.len() == 0 {
            return self.size;
        }

        let mut total = 0;
        for file in self.files {
            total += file.directory_size();
        }
        return total;
    }

    fn get_directories_at_most(self, limit: usize) -> Vec<usize> {
        if self.files.len() == 0 {
            return Vec::new();
        }

        let mut dirs = Vec::new();
        if self.directory_size() <= limit {
            dirs.push(self.directory_size())
        }

        for file in self.files {
            dirs.append(&mut file.get_directories_at_most(limit));
        }

        return dirs;
    }

    fn directory_sizes(self) -> Vec<usize> {
        if self.files.len() == 0 {
            return Vec::new();
        }

        let mut dirs: Vec<usize> = vec![self.directory_size()];

        for file in self.files {
            dirs.append(&mut file.directory_sizes());
        }

        return dirs;
    }
}

fn part1(root: &Directory) {
    let dirs = root.get_directories_at_most(100_000);

    let mut total = 0;
    for size in dirs {
        total += size;
    }

    println!("{}", total);
}

fn part2(root: &Directory) {
    let sizes = root.directory_sizes().sort();

    let total_size = 70_000_000;
    let target_size = 30_000_000;

    let root_size = root.directory_size();
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

    let mut root = Directory::new("/".to_string(), 0);

    let mut cur = &root;

    let mut lines = contents.lines().collect::<Vec<_>>();
    lines.pop();

    for (index, line) in lines.iter().enumerate() {
        if line.as_bytes()[0] as char == '$' {
            let args = line.split(" ").collect::<Vec<_>>();
            match args[1] {
                "cd" => {
                    let command = args[args.len() - 1];
                    match command {
                        ".." => {
                            cur = cur.parent.unwrap();
                        }
                        "/" => {
                            cur = &root;
                        }
                        c => {
                            let found = cur.find_file(command.to_string());
                            if found != None {
                                cur = found.unwrap();
                            } else {
                                let mut new_file = cur.add_file(command.to_string(), 0);
                                new_file.parent = Some(cur);
                                cur = new_file;
                            }
                        }
                    }
                }
                "ls" => {
                    for listing in lines.iter().skip(index + 1) {
                        if listing.as_bytes()[0] as char == '$' {
                            break;
                        }

                        let entry = listing.split(" ").collect();

                        assert_eq!(entry.len(), 2);

                        if entry[0] == "dir" {
                            let mut new_file = cur.add_file(entry[1], 0);
                            new_file.parent = Some(cur);
                        } else {
                            cur.add_file(entry[1], usize::from_str(entry[0]).unwrap());
                        }
                    }
                }
                _ => panic!("Unknown command"),
            }
        }
    }

    part1(&root);
    part2(&root);

    Ok(())
}
