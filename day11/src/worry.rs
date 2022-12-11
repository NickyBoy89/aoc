use num_bigint::BigUint;
use std::{collections::HashMap, time::Instant};

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

    pub fn is_divisible_by(&self, factor: usize) -> bool {
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

    pub fn add(&mut self, other: &Worry) -> &mut Worry {
        let now = Instant::now();
        self.number += &other.number;
        self.update_factors();
        println!("Add took {:?}", now.elapsed());
        self
    }

    pub fn mul(&mut self, other: &Worry) -> &mut Worry {
        let now = Instant::now();
        self.number *= &other.number;
        self.update_factors();
        println!("Mul took {:?}", now.elapsed());
        self
    }

    pub fn div(&mut self, other: &Worry) -> &mut Worry {
        let now = Instant::now();
        self.number /= &other.number;
        self.update_factors();
        println!("Div took {:?}", now.elapsed());
        self
    }
}
