use std::io::prelude::*;
use std::net::TcpStream;
use std::thread;
use std::time::Duration;
use std::io::Write;

use crate::{
    config::{server_addr, with_header, HeaderKind},
    login::Credential,
    packet::cross_server_war,
};

mod config;
mod login;
mod packet;

/// Auto write to a pre-config connection stream.
fn place_bet(credential: Credential, id: u32, amount: u32) -> Result<(), std::io::Error> {
    let server = server_addr(credential.cfg_server);

    let login_msg = with_header(HeaderKind::Login, credential.login());
    let enter_msg = with_header(HeaderKind::Enter, credential.enter_world());
    let bet_msg = with_header(HeaderKind::Normal, cross_server_war(id, amount));

    let mut stream = TcpStream::connect(server)?;
    println!("Connection established: {:?}", server);

    stream.write(&login_msg)?;
    stream.read(&mut [0; 256])?;

    stream.write(&enter_msg)?;
    stream.read(&mut [0; 80])?;

    thread::sleep(Duration::from_secs(3));

    // Performance: 7x accepted
    for _ in 1..100 {
        stream.write(&bet_msg)?;
    }

    Ok(())
}

fn prompt(name:&str) -> String {
    let mut line = String::new();
    print!("{}", name);
    std::io::stdout().flush().unwrap();
    std::io::stdin().read_line(&mut line).expect("Error: Could not read a line");
 
    return line.trim().to_string()
}   

fn main() -> Result<(), std::io::Error> {
    // let credentials = Credential::cfg_credentials()?;

    // for credential in credentials {
    //     thread::spawn(|| write_stream(credential));
    // }

    let input = prompt("id:> ");
    let pid = input.parse::<u32>().unwrap();

    let input = prompt("amount:> ");
    let amount = input.parse::<u32>().unwrap();

    // Single thread
    let credential = Credential::cfg_credential()?;
    place_bet(credential, pid, amount)?;

    thread::sleep(Duration::from_secs(10));
    println!("Packets sent, please wait 10 secs!");
    Ok(())
}
