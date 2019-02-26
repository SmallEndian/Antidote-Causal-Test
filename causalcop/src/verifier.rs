use super::closure::Closure;
use super::history::{Event, History};

use super::{Map, Set};

pub struct CausalConsistency;

pub type EventId = (usize, usize, usize);
pub type TransactionId = (usize, usize);
pub type SessionId = usize;

impl CausalConsistency {
    pub fn verify(history: &History) -> Option<Vec<EventId>> {
        let mut wr_rel = Map::<u64, Map<TransactionId, Set<TransactionId>>>::default();

        {
            let mut write_map = Map::default();
            let mut external_read = Vec::new();

            for (session_i, session) in history.iter().enumerate() {
                for (transaction_i, transaction) in session.iter().enumerate() {
                    let mut last = Map::default();
                    let mut written = Set::default();
                    for event in transaction.iter() {
                        match event {
                            Event::Write(variable, value) => {
                                last.insert(variable, value);
                                written.insert(variable);
                            }
                            Event::Read(variable, value) => match last.get(variable) {
                                Some(&v) => assert_eq!(v, value),
                                None => {
                                    last.insert(variable, value);
                                    external_read.push((
                                        session_i + 1,
                                        transaction_i,
                                        variable,
                                        value,
                                    ));
                                }
                            },
                        }
                    }
                    written.drain().for_each(|variable| {
                        write_map
                            .insert((variable, last[variable]), (session_i + 1, transaction_i));
                    });
                }
            }

            write_map.iter().for_each(|(&(variable, _), &t)| {
                let entry_variable = wr_rel.entry(*variable).or_insert_with(Map::default);
                entry_variable.entry(t).or_insert_with(Set::default);
            });

            for (session_i, transaction_i, variable, value) in external_read {
                let &(w_session_i, w_transaction_i) = if value == &0 {
                    &(0, 0)
                } else {
                    write_map
                        .get(&(&variable, &value))
                        .expect("no write for this value")
                };

                let entry_variable = wr_rel.entry(*variable).or_insert_with(Map::default);
                let entry_transaction = entry_variable
                    .entry((w_session_i, w_transaction_i))
                    .or_insert_with(Set::default);
                entry_transaction.insert((session_i, transaction_i));
            }
        }

        let mut wr_so_closure = Closure::<TransactionId>::default();

        wr_rel.iter().for_each(|(_, tmap)| {
            tmap.iter()
                .for_each(|(t1, t2s)| t2s.iter().for_each(|t2| wr_so_closure.add(t1, t2)))
        });

        for (session_i_d, session) in history.iter().enumerate() {
            let session_i = session_i_d + 1;
            wr_so_closure.add(&(0, 0), &(session_i, 0));
            (1..session.len()).for_each(|transaction_i| {
                wr_so_closure.add(&(session_i, transaction_i - 1), &(session_i, transaction_i))
            });
        }

        assert!(!wr_so_closure.has_cycle());

        let mut co = wr_so_closure.clone();

        for (_, tmap) in wr_rel.iter() {
            for (t1, t2s) in tmap.iter() {
                for (t3, _) in tmap.iter() {
                    if t1 != t3 && t2s.iter().any(|t2| wr_so_closure.contains(t3, t2)) {
                        co.add(&t3, &t1);
                    }
                }
            }
        }

        if co.has_cycle() {
            Some(Vec::new())
        } else {
            None
        }
    }
}
