use crate::Directions;

#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Tile {
    height: usize,
    explored: Directions,
    pub is_start: bool,
    pub is_end: bool,
}

impl Tile {
    pub fn blank() -> Tile {
        Tile {
            height: 0,
            explored: Directions {
                up: false,
                down: false,
                left: false,
                right: false,
            },
            is_start: false,
            is_end: false,
        }
    }
    pub fn parse(input: char) -> Tile {
        if input == 'S' {
            return Tile {
                height: 0,
                is_start: true,
                is_end: false,
                explored: Directions {
                    up: false,
                    down: false,
                    left: false,
                    right: false,
                },
            };
        } else if input == 'E' {
            return Tile {
                height: 25,
                is_start: false,
                is_end: true,
                explored: Directions {
                    up: false,
                    down: false,
                    left: false,
                    right: false,
                },
            };
        }
        Tile {
            height: input as usize - 'a' as usize,
            is_start: false,
            is_end: false,
            explored: Directions {
                up: false,
                down: false,
                left: false,
                right: false,
            },
        }
    }

    pub fn heightdiff(&self, other: &Tile) -> isize {
        other.height as isize - self.height as isize
    }

    fn explored(&self) -> bool {
        self.explored.up == true
            && self.explored.down == true
            && self.explored.left == true
            && self.explored.right == true
    }
}
