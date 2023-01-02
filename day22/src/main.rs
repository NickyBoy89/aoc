use std::str::FromStr;
use std::{fs::File, io::Read};

#[derive(Debug, Copy, Clone)]
enum Tile {
    Blocked,
    Open,
    Void,
}

struct Map {
    width: usize,
    height: usize,
    tiles: Vec<Vec<Tile>>,
}

impl Map {
    fn parse(input: &str) -> Map {
        let lines = input.lines().collect::<Vec<_>>();
        let width = lines.iter().map(|line| line.len()).max().unwrap();
        let height = lines.len();

        println!("Width: {}, Height: {}", width, height);

        let mut map = vec![vec![Tile::Blocked; width]; height];

        for (ri, row) in lines.iter().enumerate() {
            let chars = row.chars().collect::<Vec<_>>();
            for ci in 0..width {
                if ci > chars.len() - 1 {
                    map[ri][ci] = Tile::Void;
                } else {
                    match chars[ci] {
                        '.' => map[ri][ci] = Tile::Open,
                        ' ' => map[ri][ci] = Tile::Void,
                        '#' => (),
                        _ => panic!("Unknown tile type"),
                    }
                }
            }
        }

        Map {
            width,
            height,
            tiles: map,
        }
    }

    fn display(&self) {
        for ri in 0..self.height {
            for ci in 0..self.width {
                match self.tiles[ri][ci] {
                    Tile::Blocked => {
                        print!("#");
                    }
                    Tile::Open => {
                        print!(".");
                    }
                    Tile::Void => {
                        print!(" ");
                    }
                }
            }
            println!();
        }
    }

    fn display_with_point(&self, point: &Point) {
        for ri in 0..self.height {
            for ci in 0..self.width {
                if ri == point.y as usize && ci == point.x as usize {
                    print!("@");
                } else {
                    match self.tiles[ri][ci] {
                        Tile::Blocked => {
                            print!("#");
                        }
                        Tile::Open => {
                            print!(".");
                        }
                        Tile::Void => {
                            print!(" ");
                        }
                    }
                }
            }
            println!();
        }
    }

    fn tile_at(&self, point: &Point) -> Tile {
        self.tiles[point.y as usize][point.x as usize]
    }

    fn is_solid(&self, point: &Point) -> bool {
        matches!(self.tile_at(point), Tile::Blocked)
    }

    fn range_of_row(&self, row: usize) -> (usize, usize) {
        println!("Getting range of row {}", row);
        let start_x = self.tiles[row]
            .iter()
            .position(|tile| !matches!(tile, Tile::Void))
            .unwrap();
        let end_x = (self.width - 1)
            - self.tiles[row]
                .iter()
                .rev()
                .position(|tile| !matches!(tile, Tile::Void))
                .unwrap_or(0);
        (start_x, end_x)
    }

    fn range_of_col(&self, col: usize) -> (usize, usize) {
        println!("Getting range of col {}", col);
        let start_y = self
            .tiles
            .iter()
            .position(|row| !matches!(row[col], Tile::Void))
            .unwrap();
        let end_y = (self.height - 1)
            - self
                .tiles
                .iter()
                .skip(start_y)
                .position(|row| matches!(row[col], Tile::Void))
                .unwrap_or(0);
        let result = (start_y, end_y);
        println!("Col {} starts and ends at {:?}", col, result);
        result
    }
}

#[derive(Debug)]
struct Point {
    x: isize,
    y: isize,
}

impl Point {
    fn face_number(&self) -> usize {
        0
    }
}

#[derive(Debug)]
enum Direction {
    Right,
    Left,
    Up,
    Down,
}

impl Direction {
    fn clockwise(&mut self) {
        *self = match self {
            Direction::Right => Direction::Down,
            Direction::Left => Direction::Up,
            Direction::Down => Direction::Left,
            Direction::Up => Direction::Right,
        }
    }

    fn counterclockwise(&mut self) {
        *self = match self {
            Direction::Right => Direction::Up,
            Direction::Left => Direction::Down,
            Direction::Down => Direction::Right,
            Direction::Up => Direction::Left,
        }
    }

    fn facing_value(&self) -> usize {
        match self {
            Direction::Right => 0,
            Direction::Down => 1,
            Direction::Left => 2,
            Direction::Up => 3,
        }
    }
}

#[derive(Debug)]
enum Movement {
    Move(usize),
    L,
    R,
}

fn part1(contents: &mut String) {
    let (map, directions) = contents.split_once("\n\n").unwrap();
    let map = Map::parse(map);

    map.display();

    let mut movements = Vec::new();

    let mut directions = directions.trim();
    while directions.len() > 0 {
        let next_rotation = directions.chars().position(|c| c == 'L' || c == 'R');
        match next_rotation {
            Some(pos) => {
                movements.push(Movement::Move(
                    usize::from_str(&directions[0..pos]).unwrap(),
                ));
                match directions.as_bytes()[pos] as char {
                    'L' => movements.push(Movement::L),
                    'R' => movements.push(Movement::R),
                    other => panic!("Unknown direction: {}", other),
                }

                directions = &directions[pos + 1..];
            }
            None => {
                movements.push(Movement::Move(usize::from_str(directions).unwrap()));
                directions = "";
            }
        }
    }

    println!("Movements: {:?}", movements);

    let (start_x, _) = map.range_of_row(0);

    let mut current_point = Point {
        x: start_x as isize,
        y: 0,
    };

    println!("Starting at {:?}", current_point);

    let mut direction = Direction::Right;

    for movement in movements {
        match movement {
            Movement::L => {
                println!("Rotating counterclockwise");
                direction.counterclockwise()
            }
            Movement::R => {
                println!("Rotating clockwise");
                direction.clockwise()
            }
            Movement::Move(distance) => {
                println!("Moving {} units in direction {:?}", distance, direction);
                for _ in 0..distance {
                    let mut new_point = match direction {
                        Direction::Right => Point {
                            x: current_point.x + 1,
                            y: current_point.y,
                        },
                        Direction::Left => Point {
                            x: current_point.x - 1,
                            y: current_point.y,
                        },
                        Direction::Up => Point {
                            x: current_point.x,
                            y: current_point.y - 1,
                        },
                        Direction::Down => Point {
                            x: current_point.x,
                            y: current_point.y + 1,
                        },
                    };

                    if new_point.x < 0 || new_point.x >= map.width as isize {
                        let (min_x, max_x) = map.range_of_row(new_point.y as usize);
                        if new_point.x < 0 {
                            new_point.x = max_x as isize;
                        } else {
                            new_point.x = min_x as isize;
                        }
                    }

                    if new_point.y < 0 || new_point.y >= map.height as isize {
                        let (min_y, max_y) = map.range_of_col(new_point.x as usize);
                        if new_point.y < 0 {
                            new_point.y = max_y as isize;
                        } else {
                            new_point.y = min_y as isize;
                        }
                    }

                    if !map.is_solid(&new_point) {
                        current_point = new_point;
                    }
                }

                println!("Ended at {:?}", current_point);
                //map.display_with_point(&current_point);

                //let mut temp = String::new();
                //std::io::stdin().read_line(&mut temp);
            }
        }
    }

    for i in 0..map.width {
        map.range_of_col(i);
    }

    println!("Final point is at: {:?}", current_point);

    println!(
        "Final password: {}",
        1000 * (current_point.y as usize + 1)
            + 4 * (current_point.x as usize + 1)
            + direction.facing_value()
    );
}

fn part2(contents: &mut String) {}

fn main() -> std::io::Result<()> {
    let mut f = File::open("input")?;
    let mut contents = String::new();
    f.read_to_string(&mut contents)?;

    part1(&mut contents);
    part2(&mut contents);

    Ok(())
}
