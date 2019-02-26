use super::{Map, Set};

use std::cmp::Eq;
use std::fmt::Debug;
use std::hash::Hash;

use std::iter::once;

#[derive(Debug, Default, Clone)]
pub struct Closure<T>
where
    T: Debug + Hash + Eq + Copy,
{
    pub edge: Map<T, Set<T>>,
    pub back_edge: Map<T, Set<T>>,
}

impl<T> Closure<T>
where
    T: Debug + Hash + Eq + Copy,
{
    pub fn add(&mut self, u: &T, v: &T) {
        let mut new_edges: Map<T, Set<T>> = Default::default();
        let mut new_back_edges: Map<T, Set<T>> = Default::default();

        {
            let pre_us_empty = Default::default();
            let post_vs_empty = Default::default();

            let pre_us = self.back_edge.get(u).unwrap_or_else(|| &pre_us_empty);
            let post_vs = self.edge.get(v).unwrap_or_else(|| &post_vs_empty);

            for &pre_u in pre_us.iter().chain(once(u)) {
                let entry = new_edges.entry(pre_u).or_insert_with(Set::default);
                entry.extend(post_vs.iter().chain(once(v)));
            }

            for &post_v in post_vs.iter().chain(once(v)) {
                let entry = new_back_edges.entry(post_v).or_insert_with(Set::default);
                entry.extend(pre_us.iter().chain(once(u)));
            }
        }

        for (&u1, vs1) in new_edges.iter() {
            let entry = self.edge.entry(u1).or_insert_with(Default::default);
            entry.extend(vs1.iter())
        }

        for (&v1, us1) in new_back_edges.iter() {
            let entry = self.back_edge.entry(v1).or_insert_with(Default::default);
            entry.extend(us1.iter())
        }
    }

    pub fn contains(&mut self, u: &T, v: &T) -> bool {
        self.edge.get(u).map(|vs| vs.contains(v)) == Some(true)
    }

    pub fn has_cycle(&self) -> bool {
        self.edge.iter().any(|(u, vs)| vs.contains(u))
    }
}
