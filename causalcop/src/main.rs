#[macro_use]
extern crate nom;
extern crate hashbrown;

pub mod closure;
pub mod history;
pub mod parser;
pub mod verifier;

use std::fs::File;
use std::io::Read;

use verifier::CausalConsistency;

use parser::parse_history;

type Map<K, V> = hashbrown::HashMap<K, V>;
type Set<T> = hashbrown::HashSet<T>;

// type Map<K, V> = std::collections::HashMap<K, V>;
// type Set<T> = std::collections::HashSet<T>;

fn help() {
    println!(
        r#"Usage:
    copcc [file]       read specified file
or: copcc -            read from stdin
"#
    );
    std::process::exit(3);
}

fn main() {
    match std::env::args().nth(1) {
        None => help(),
        Some(ref s) if s == "-h" || s == "--help" => help(),
        Some(path) => {
            let mut reader: Box<Read> = if path == "-" {
                Box::new(std::io::stdin())
            } else {
                Box::new(File::open(path).unwrap())
            };

            let history = {
                let mut contents = Vec::new();
                reader.read_to_end(&mut contents).unwrap();
                let parsed = parse_history(&contents);
                parsed.expect("error whole parsing").1
            };

            std::process::exit(if CausalConsistency::verify(&history).is_none() {
                println!("true");
                0
            } else {
                println!("false");
                1
            });
        }
    }
}
