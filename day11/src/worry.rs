use num_bigint::BigUint;
use std::collections::HashMap;

/// Worry is an arbritrary-precision datatype that represents numbers as a
/// series of factors
///
/// Multiplication adds a factor, while division takes one away
/// Adding causes all the factors to be calculated again
#[derive(Clone, Debug)]
pub struct Worry {
    number: BigUint,
    input_factors: HashMap<usize, bool>,
    dirty: bool,
}

impl Worry {
    pub fn from(n: usize) -> Worry {
        Worry {
            number: BigUint::from(n),
            input_factors: HashMap::new(),
            dirty: true,
        }
    }

    pub fn from_with_factors(n: usize, factors: &[usize]) -> Worry {
        let mut new_worry = Worry {
            number: BigUint::from(n),
            input_factors: HashMap::from_iter(
                factors.iter().copied().map(|factor| (factor, false)),
            ),
            dirty: false,
        };
        new_worry.update_factors();
        new_worry
    }

    pub fn is_divisible_by(self, factor: usize) -> bool {
        if self.dirty {
            panic!("Tried to get factors of dirty number");
        }
        self.input_factors[&factor]
    }

    pub fn add_factors(&mut self, factors: &[usize]) {
        self.input_factors =
            HashMap::from_iter(factors.iter().copied().map(|factor| (factor, false)))
    }

    fn update_factors(&mut self) {
        self.dirty = false;
        let factors = self.input_factors.clone();
        for factor in factors.keys() {
            *self.input_factors.get_mut(factor).unwrap() =
                self.number.clone() % factor == BigUint::from(0 as usize);
        }
    }

    pub fn add(&self, other: Worry) -> Worry {
        let mut new_worry = Worry {
            number: self.number.clone() + other.number,
            input_factors: self.input_factors.clone(),
            dirty: true,
        };

        new_worry.update_factors();
        new_worry
    }

    pub fn mul(&self, other: Worry) -> Worry {
        let mut new_worry = Worry {
            number: self.number.clone() * other.number,
            input_factors: self.input_factors.clone(),
            dirty: true,
        };

        new_worry.update_factors();
        new_worry
    }
}
