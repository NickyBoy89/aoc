#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct SequentialRange {
    start: isize,
    // Inclusive
    end: isize,
}

impl SequentialRange {
    pub fn from_to(start: isize, end: isize) -> SequentialRange {
        SequentialRange { start, end }
    }

    fn size(&self) -> usize {
        self.end as usize - self.start as usize
    }
}

#[derive(Debug, Clone)]
pub struct Range {
    pub parts: Vec<SequentialRange>,
}

impl Range {
    pub fn new() -> Range {
        Range { parts: Vec::new() }
    }

    pub fn ranges(&self) -> usize {
        self.parts.len()
    }

    fn remove_contained_fully(&mut self, comparison: &SequentialRange) {
        self.parts.retain(|range| {
            range == comparison || range.start < comparison.start || range.end > comparison.end
        });
    }

    pub fn first_nonoverlap(&self) -> Option<isize> {
        if self.ranges() == 1 {
            return None;
        }

        Some(self.parts[0].end + 1)
    }

    pub fn add_range(&mut self, range: &SequentialRange) {
        if range.size() == 0 {
            return;
        }

        // min_range is the position of the range where the current range starts in
        let min_range = self
            .parts
            .iter()
            .position(|r| range.start > r.start && range.start <= r.end);

        let max_range = self
            .parts
            .iter()
            .position(|r| range.end >= r.start && range.end < r.end);

        if let Some(min_range) = min_range {
            if let Some(max_range) = max_range {
                // Contained in the same element
                if min_range == max_range {
                    return;
                }

                self.parts[min_range].end = self.parts[max_range].end;
                self.parts.remove(max_range);
                let new_part = self.parts[min_range];
                self.remove_contained_fully(&new_part);
            } else {
                self.parts[min_range].end = range.end;
                let new_part = self.parts[min_range];
                self.remove_contained_fully(&new_part);
            }
        } else {
            if let Some(max_range) = max_range {
                self.parts[max_range].start = range.start;
                let new_part = self.parts[max_range];
                self.remove_contained_fully(&new_part);
            } else {
                self.parts.push(*range);
                self.remove_contained_fully(range)
            }
        }
    }
}
