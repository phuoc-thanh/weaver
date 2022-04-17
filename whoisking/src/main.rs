use std::io::prelude::*;
use std::net::TcpStream;
use std::thread;
use std::time::Duration;

use crate::{
    config::{decode_hex, server_addr, with_header, HeaderKind},
    login::Credential,
};

mod config;
mod login;

// PhuBich: 9000012
// TrieuLinh s1: 101
// mauvi s1: 355
// BlackGO s5: 5000245
// Cocain s5: 5000009
// LamQuang s9: 9000138
fn main() {
    let server01 = server_addr(1);

    let credential = Credential::cfg_credential().unwrap();

    let login_msg = with_header(HeaderKind::Login, credential.login());
    let enter_msg = with_header(HeaderKind::Enter, credential.enter_world());

    let space = "20";
    let null = "00";

    // let hard_bytes = "0008ff2300";
    let crosssv_war = "63726f73737365727665727761722062657474696e67";
    let bet_candidate = "35303030323435";
    let bet_amount = "313230303030"; //120k
    let final_bet = format!(
        "{}{}{}{}{}{}",
        crosssv_war, space, bet_candidate, space, bet_amount, null
    );
    let bet_data = decode_hex(&format!("{}", final_bet))
        .unwrap()
        .into_boxed_slice();
    let bet_msg = with_header(HeaderKind::Normal, bet_data);

    // let rank_reward = "160003ff120072616e6b4c69737420726577617264203100";
    // let rmsg = hex::decode(rank_reward).unwrap();

    let mut stream = TcpStream::connect(server01).expect("Cannot bind tcp stream");

    stream.write(&login_msg).expect("Cannot login");
    stream.read(&mut [0; 256]).expect("Cannot recv login msg");

    stream.write(&enter_msg).expect("Cannot enter");
    stream.read(&mut [0; 80]).expect("Cannot recv enter msg");

    thread::sleep(Duration::from_secs(3));

    // performance: 70 accept
    for _ in 1..100 {
        stream.write(&bet_msg).expect("bet failed");
    }

    println!("End of program!");
}

#[cfg(test)]
mod tests {

    #[test]
    fn test_into_bytes() {
        let betting = "crossserverwar betting".to_owned();
        let raw_bytes = betting.as_bytes();
        let expected = vec![
            99, 114, 111, 115, 115, 115, 101, 114, 118, 101, 114, 119, 97, 114, 32, 98, 101, 116,
            116, 105, 110, 103,
        ]
        .into_boxed_slice();

        assert_eq!(raw_bytes, &expected[..]);
    }
}
