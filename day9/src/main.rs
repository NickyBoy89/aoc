use std::fs;

enum Data {
    FreeSpace(usize),
    Files(usize, usize),
}

struct DiskMap {
    data: Vec<Data>,
}

impl DiskMap {
    fn parse(str: &str) -> Self {
        let mut data = Vec::new();

        let mut cur_id = 0;
        let mut is_file = true;

        for c in str.chars() {
            if c == '\n' {
                continue;
            }
            data.push(if is_file {
                let res = Data::Files(cur_id, c.to_digit(10).unwrap() as usize);
                cur_id += 1;
                res
            } else {
                Data::FreeSpace(c.to_digit(10).unwrap() as usize)
            });
            is_file = !is_file;
        }

        Self { data }
    }

    fn pretty_print(&self) {
        for block in self.data.iter() {
            match block {
                Data::FreeSpace(amount) => print!("{}", ".".repeat(*amount)),
                Data::Files(id, size) => print!("{}", id.to_string().repeat(*size)),
            }
        }
        println!();
    }

    fn compress(&mut self) {
        let mut free = 0;
        let mut to_compress = self.data.len() - 1;

        while to_compress > free {
            let free_space = match self.data[free] {
                Data::FreeSpace(size) => size,
                _ => {
                    free += 1;
                    continue;
                }
            };

            let (file_id, file_size) = match self.data[to_compress] {
                Data::Files(id, size) => (id, size),
                _ => {
                    to_compress -= 1;
                    continue;
                }
            };

            // The file is too large, fill up the entire free space and move on
            if file_size > free_space {
                self.data[free] = Data::Files(file_id, free_space);
                free += 1;
                self.data[to_compress] = Data::Files(file_id, file_size - free_space);
                self.data
                    .insert(to_compress + 1, Data::FreeSpace(free_space));
            } else if file_size < free_space {
                // File leaves some free space, insert some more
                self.data[free] = Data::Files(file_id, file_size);
                free += 1;
                self.data
                    .insert(free, Data::FreeSpace(free_space - file_size));
                to_compress += 1;
                self.data[to_compress] = Data::FreeSpace(file_size);
                to_compress -= 1;
            } else {
                self.data[free] = Data::Files(file_id, file_size);
                free += 1;
                self.data[to_compress] = Data::FreeSpace(file_size);
                to_compress -= 1;
            }
        }
    }

    fn compress_entire_files(&mut self) {
        let mut to_compress = self.data.len() - 1;

        while to_compress > 1 {
            let mut free = 0;

            while free < to_compress {
                let free_space = match self.data[free] {
                    Data::FreeSpace(size) => size,
                    _ => {
                        free += 1;
                        continue;
                    }
                };
                let (file_id, file_size) = match self.data[to_compress] {
                    Data::Files(id, size) => (id, size),
                    _ => {
                        to_compress -= 1;
                        continue;
                    }
                };

                if free_space >= file_size {
                    self.data[free] = Data::Files(file_id, file_size);

                    // Not an exact fit, we need to pad out the space
                    if free_space > file_size {
                        free += 1;
                        self.data
                            .insert(free, Data::FreeSpace(free_space - file_size));
                        to_compress += 1;
                    }

                    self.data[to_compress] = Data::FreeSpace(file_size);

                    break;
                }

                free += 1;
            }

            to_compress -= 1;
        }
    }

    fn checksum(&self) -> usize {
        let mut cur_index = 0;
        self.data
            .iter()
            .map(|block| match block {
                Data::FreeSpace(size) => {
                    cur_index += size;
                    0
                }
                Data::Files(id, size) => {
                    let mut subtotal = 0;
                    for _ in 0..*size {
                        subtotal += cur_index * id;
                        cur_index += 1;
                    }
                    subtotal
                }
            })
            .sum()
    }
}

fn part1(input: &String) {
    let mut disk = DiskMap::parse(input.as_str());

    // disk.pretty_print();

    disk.compress();

    println!("Checksum: {}", disk.checksum());
}

fn part2(input: &String) {
    let mut disk = DiskMap::parse(input.as_str());

    // disk.pretty_print();

    disk.compress_entire_files();

    println!("Checksum: {}", disk.checksum());
}

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();

    part1(&input);
    part2(&input);
}
