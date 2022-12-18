use std::ops::Add;

use crate::TIME_LEFT;

#[derive(Eq, PartialEq, Clone, Copy)]
pub struct Cost {
    pub pressure_released: usize,
    pub time_remaining: usize,
}

impl Cost {
    pub fn new(n: usize) -> Cost {
        Cost {
            pressure_released: n,
            time_remaining: TIME_LEFT,
        }
    }
}

impl PartialOrd for Cost {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Cost {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.pressure_released.cmp(&other.pressure_released)
    }
}

impl Add for Cost {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Self {
            pressure_released: self.pressure_released + other.pressure_released,
            time_remaining: self.time_remaining - (TIME_LEFT - other.time_remaining),
        }
    }
}
