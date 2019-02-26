#[derive(Debug)]
pub enum Event {
    Write(u64, u64),
    Read(u64, u64),
}

pub type Transaction = Vec<Event>;
pub type Session = Vec<Transaction>;
pub type History = Vec<Session>;
