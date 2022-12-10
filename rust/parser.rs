mod json;

use crate::json::{parse_str};

use std::env;
use std::fs;

pub fn main() {
    let args = env::args().collect::<Vec<String>>();
    let print = args[1].as_str() == "-p";
    let input_file = args[2].clone();
    let json_str = fs::read_to_string(input_file).unwrap();
    match parse_str(json_str.as_str()) {
        Ok(json) => {
            if print {
                println!("{}", json)
            } else {
                println!("OK")
            }
        },
        Err((error, idx)) => println!("Error: {} at {}", error, idx),
    }
}
