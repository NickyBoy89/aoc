use std::collections::HashMap;
use std::fs;

fn get_stone(stone: usize) -> Vec<usize> {
    let digits = stone.to_string();
    if stone == 0 {
        return vec![1];
    } else if digits.len() % 2 == 0 {
        let cutoff = digits.len() / 2;
        return vec![
            digits[..cutoff].parse::<usize>().unwrap(),
            digits[cutoff..].parse::<usize>().unwrap(),
        ];
    }
    vec![stone * 2024]
}

fn part1(stones: &Vec<usize>, blinks: usize) {
    let mut stones: HashMap<usize, usize> =
        HashMap::from_iter(stones.iter().map(|&stone| (stone, 1)));

    for _ in 0..blinks {
        let mut temp = HashMap::new();
        for (stone, count) in stones.iter() {
            if *count == 0 {
                continue;
            }
            // println!("Blinking stone: {:?}", stone);
            // *stones.get_mut(&stone).unwrap() = 0;

            // if stones[&stone] == 0 {
            //     stones.remove(&stone);
            // }

            // println!("Results: {:?}", get_stone(*stone));
            for s in get_stone(*stone) {
                temp.entry(s)
                    .and_modify(|occurrences| *occurrences += count)
                    .or_insert(*count);
            }
        }

        stones = temp;
    }

    println!("Size: {}", stones.values().sum::<usize>());
}

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();

    let stones = input[..input.len() - 1]
        .split(" ")
        .map(|n| n.parse::<usize>().unwrap())
        .collect::<Vec<_>>();

    part1(&stones, 25);
    part1(&stones, 75);
}
