use std::{
    collections::{HashMap, HashSet},
    fs,
};

struct OrderingRules {
    pages_after: HashMap<usize, HashSet<usize>>,
}

impl OrderingRules {
    fn new() -> Self {
        Self {
            pages_after: HashMap::new(),
        }
    }

    fn parse_rule(&mut self, input: &str) {
        let (pre, post) = input.split_once("|").unwrap();
        let pre = pre.parse::<usize>().unwrap();
        let post = post.parse::<usize>().unwrap();

        self.pages_after.entry(pre).or_insert(HashSet::new());
        self.pages_after.get_mut(&pre).unwrap().insert(post);
    }

    fn all_pages_before(&self, page_num: usize) -> Vec<usize> {
        let mut pages_before = Vec::new();

        for (before_page, after_pages) in self.pages_after.iter() {
            if after_pages.contains(&page_num) {
                pages_before.push(*before_page);
            }
        }

        pages_before
    }

    fn correct_ordering(&self, pages: &Vec<usize>) -> Vec<usize> {
        let pages_before: HashMap<usize, Vec<usize>> = HashMap::from_iter(
            pages
                .iter()
                .map(|&page| (page, self.all_pages_before(page))),
        );

        let mut size_to_fill = pages.len() - 1;
        let mut corrected = vec![0; pages.len()];

        while size_to_fill > 0 {
            let mut found = false;
            for (page_num, before) in pages_before.iter() {
                let before = before
                    .iter()
                    .filter(|x| pages.contains(&x))
                    .collect::<Vec<_>>();

                if before.len() == size_to_fill {
                    corrected[size_to_fill] = *page_num;
                    size_to_fill -= 1;
                    found = true;
                    break;
                }
            }

            if !found {
                panic!("Ambigous rules!");
            }
        }

        corrected
    }
}

fn is_updates_valid(updates: &Vec<usize>, ordering_rules: &OrderingRules) -> bool {
    let mut seen_pages = HashSet::new();

    for &page_number in updates.iter() {
        for page in ordering_rules.all_pages_before(page_number) {
            if !updates.contains(&page) {
                continue;
            }

            if !seen_pages.contains(&page) {
                return false;
            }
        }
        seen_pages.insert(page_number);
    }

    true
}

fn part1(input: &String) {
    let (raw_ordering_rules, raw_updates) = input.split_once("\n\n").unwrap();

    let mut ordering_rules = OrderingRules::new();

    for rule in raw_ordering_rules.lines() {
        ordering_rules.parse_rule(rule);
    }

    let mut updates = Vec::new();

    for page_numbers in raw_updates.lines() {
        updates.push(
            page_numbers
                .split(",")
                .map(|n| n.parse::<usize>().unwrap())
                .collect::<Vec<_>>(),
        );
    }

    let mut total_middle_pages = 0;

    for update in updates {
        let valid = is_updates_valid(&update, &ordering_rules);
        if valid {
            total_middle_pages += update[update.len() / 2];
        }
    }

    println!("Total of middle pages was: {}", total_middle_pages);
}

fn part2(input: &String) {
    let (raw_ordering_rules, raw_updates) = input.split_once("\n\n").unwrap();

    let mut ordering_rules = OrderingRules::new();

    for rule in raw_ordering_rules.lines() {
        ordering_rules.parse_rule(rule);
    }

    let mut updates = Vec::new();

    for page_numbers in raw_updates.lines() {
        updates.push(
            page_numbers
                .split(",")
                .map(|n| n.parse::<usize>().unwrap())
                .collect::<Vec<_>>(),
        );
    }

    let mut total_middle_pages = 0;

    for update in updates {
        let valid = is_updates_valid(&update, &ordering_rules);
        if !valid {
            let update = ordering_rules.correct_ordering(&update);
            total_middle_pages += update[update.len() / 2];
        }
    }

    println!("Total of middle pages was: {}", total_middle_pages);
}

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();

    // part1(&input);
    part2(&input);
}
