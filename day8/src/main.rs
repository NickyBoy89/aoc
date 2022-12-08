use std::{fs::File, io::Read};

fn part1(contents: &mut String) {
    let mut visible = 0;

    let grid = contents
        .lines()
        .map(|line| {
            line.chars()
                .map(|c| c.to_digit(10).unwrap())
                .collect::<Vec<_>>()
        })
        .collect::<Vec<Vec<_>>>();

    let width = grid[0].len();
    let height = grid.len();

    for (ri, row) in grid.iter().enumerate() {
        for (ci, col) in row.iter().enumerate() {
            if ci == 0 || ci == width - 1 || ri == 0 || ri == height - 1 {
                visible += 1;
                continue;
            }

            let (left, right) = row.split_at(ci);

            let vertical = grid.iter().map(|row| row[ci]).collect::<Vec<_>>();

            let (up, down) = vertical.split_at(ri);

            if up.iter().max().unwrap() < col || down[1..].iter().max().unwrap() < col {
                visible += 1;
                continue;
            } else if left.iter().max().unwrap() < col || right[1..].iter().max().unwrap() < col {
                visible += 1;
                continue;
            }
        }
    }

    println!("Visible: {}", visible);
}

fn part2(contents: &mut String) {
    let mut max_scenic = 0;

    let grid = contents
        .lines()
        .map(|line| {
            line.chars()
                .map(|c| c.to_digit(10).unwrap())
                .collect::<Vec<_>>()
        })
        .collect::<Vec<Vec<_>>>();

    for (ri, row) in grid.iter().enumerate() {
        for (ci, col) in row.iter().enumerate() {
            let (left, right) = row.split_at(ci);

            let vertical = grid.iter().map(|row| row[ci]).collect::<Vec<_>>();

            let (up, down) = vertical.split_at(ri);

            let left_view = left.iter().rev().position(|tree| tree >= col);
            let right_view = right[1..].iter().position(|tree| tree >= col);
            let up_view = up.iter().rev().position(|tree| tree >= col);
            let down_view = down[1..].iter().position(|tree| tree >= col);

            let left_score = if left_view == None {
                left.iter().len()
            } else {
                left_view.unwrap() + 1
            };

            let right_score = if right_view == None {
                right[1..].iter().len()
            } else {
                right_view.unwrap() + 1
            };

            let up_score = if up_view == None {
                up.iter().len()
            } else {
                up_view.unwrap() + 1
            };

            let down_score = if down_view == None {
                down[1..].iter().len()
            } else {
                down_view.unwrap() + 1
            };

            let scenic_score = left_score * right_score * up_score * down_score;

            if scenic_score > max_scenic {
                max_scenic = scenic_score;
            }
        }
    }

    println!("Max scenic: {}", max_scenic);
}

fn main() -> std::io::Result<()> {
    let mut f = File::open("input")?;
    let mut contents = String::new();
    f.read_to_string(&mut contents)?;

    part1(&mut contents);
    part2(&mut contents);

    Ok(())
}
