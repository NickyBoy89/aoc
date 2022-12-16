use crate::range::SequentialRange;
use crate::Point;

#[derive(Debug)]
pub struct Sensor {
    pub position: Point,
    pub closest_beacon: Point,
}

impl Sensor {
    pub fn parse(input: &str) -> Sensor {
        let colon_ind = input.find(":").unwrap();
        let sensor_pos = Point::parse(&input["Sensor at ".len()..colon_ind]);

        let filter_str = "closest beacon is at ";
        let beacon_pos = Point::parse(&input[input.find(filter_str).unwrap() + filter_str.len()..]);

        Sensor {
            position: sensor_pos,
            closest_beacon: beacon_pos,
        }
    }

    pub fn distance_to_beacon(&self) -> usize {
        self.position.manhattan_distance(&self.closest_beacon)
    }

    pub fn point_is_within(&self, point: &Point) -> bool {
        if point == &self.closest_beacon {
            return false;
        }
        self.position.manhattan_distance(point) <= self.distance_to_beacon()
    }

    pub fn range_at_row(&self, row: isize) -> SequentialRange {
        let beacon_dist = self.distance_to_beacon() as isize;
        let dist_from_row = self.position.y.abs_diff(row) as isize;

        SequentialRange::from_to(
            self.position.x - (beacon_dist - isize::min(dist_from_row, beacon_dist)),
            self.position.x + (beacon_dist - isize::min(dist_from_row, beacon_dist)),
        )
    }
}

impl PartialOrd for Sensor {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Sensor {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.position.x.cmp(&other.position.x)
    }
}

impl PartialEq for Sensor {
    fn eq(&self, other: &Self) -> bool {
        self.position.x == other.position.x
    }
}

impl Eq for Sensor {}
