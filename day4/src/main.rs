use std::fs;

static MATCH_SIZE: usize = 4;

fn build_iter(
    startx: usize,
    xdiff: isize,
    starty: usize,
    ydiff: isize,
    grid: &Vec<String>,
    maxes: (usize, usize),
) -> impl Iterator<Item = Option<char>> + use<'_> {
    let mut indexes = Vec::new();

    let (m, n) = maxes;

    let mut cur_x = startx as isize;
    let mut cur_y = starty as isize;

    for _ in 0..MATCH_SIZE {
        indexes.push((cur_x, cur_y));
        cur_x += xdiff;
        cur_y += ydiff;
    }

    indexes.into_iter().map(move |(x, y)| {
        if x < 0 || x >= n as isize {
            return None;
        } else if y < 0 || y >= m as isize {
            return None;
        } else {
            // println!("Index is x: {:?}, y: {:?}", x, y);
            return Some(grid[y as usize].as_bytes()[x as usize] as char);
        }
    })
}

fn part1(input: &String) {
    let grid = input
        .lines()
        .map(|x| x.to_string())
        .collect::<Vec<String>>();

    let m = grid.len();
    let n = grid[0].len();

    let mut found = 0;

    for (ri, row) in input.lines().enumerate() {
        for (ci, col) in row.chars().enumerate() {
            if col == 'X' {
                let forward = build_iter(ci, 1, ri, 0, &grid, (m, n)).collect::<Vec<_>>();
                let back = build_iter(ci, -1, ri, 0, &grid, (m, n)).collect::<Vec<_>>();
                let up = build_iter(ci, 0, ri, 1, &grid, (m, n)).collect::<Vec<_>>();
                let down = build_iter(ci, 0, ri, -1, &grid, (m, n)).collect::<Vec<_>>();
                let ur = build_iter(ci, 1, ri, 1, &grid, (m, n)).collect::<Vec<_>>();
                let ul = build_iter(ci, -1, ri, 1, &grid, (m, n)).collect::<Vec<_>>();
                let dr = build_iter(ci, 1, ri, -1, &grid, (m, n)).collect::<Vec<_>>();
                let dl = build_iter(ci, -1, ri, -1, &grid, (m, n)).collect::<Vec<_>>();

                let checks = vec![forward, back, up, down, ur, ul, dr, dl];

                for potential in checks.iter() {
                    let potential = potential
                        .iter()
                        .take_while(|x| x.is_some())
                        .map(|x| x.unwrap())
                        .collect::<Vec<_>>();
                    if potential.len() != 4 {
                        continue;
                    }

                    if String::from_iter(potential) == "XMAS" {
                        found += 1;
                    }
                }
            }
        }
    }

    println!("Total found: {:?}", found);
}

fn check_bounds(pos: (isize, isize), maxx: usize, maxy: usize) -> bool {
    let x = pos.0;
    let y = pos.1;

    if x < 0 || x >= maxx as isize {
        return false;
    } else if y < 0 || y >= maxy as isize {
        return false;
    }
    true
}

fn fetch_grid(pos: (isize, isize), grid: &Vec<Vec<char>>) -> char {
    grid[pos.1 as usize][pos.0 as usize]
}

fn part2(input: &String) {
    let grid = input
        .lines()
        .map(|x| x.chars().collect::<Vec<_>>())
        .collect::<Vec<_>>();

    let m = grid.len();
    let n = grid[0].len();

    let mut found = 0;

    for (ri, row) in input.lines().enumerate() {
        for (ci, col) in row.chars().enumerate() {
            if col == 'A' {
                let dr = ((-1 as isize, -1 as isize), (1, 1));
                let ur = ((-1 as isize, 1), (1, -1 as isize));

                let mut mas_count = 0;

                // Check if the indexes are invalid
                for shape in vec![dr, ur] {
                    let (start, end) = shape;
                    let start = (start.0 + ci as isize, start.1 + ri as isize);
                    let end = (end.0 + ci as isize, end.1 + ri as isize);

                    if !check_bounds(start, n, m) || !check_bounds(end, n, m) {
                        break;
                    }
                    match fetch_grid(start, &grid) {
                        'M' => match fetch_grid(end, &grid) {
                            'S' => mas_count += 1,
                            _ => break,
                        },
                        'S' => match fetch_grid(end, &grid) {
                            'M' => mas_count += 1,
                            _ => break,
                        },
                        _ => break,
                    };
                }

                if mas_count == 2 {
                    found += 1;
                }
            }
        }
    }

    println!("Total found: {:?}", found);
}

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();

    // part1(&input);
    part2(&input);
}
