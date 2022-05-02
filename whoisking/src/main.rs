use std::io::prelude::*;
use std::net::TcpStream;
use std::thread;
use std::time::Duration;

use crate::{
    config::{server_addr, with_header, HeaderKind},
    login::Credential,
    packet::cross_server_war,
};

mod config;
mod login;
mod packet;

/// Auto write to a pre-config connection stream.
fn write_stream(credential: Credential) -> Result<(), std::io::Error> {
    let server = server_addr(8000 + credential.cfg_server);

    let login_msg = with_header(HeaderKind::Login, credential.login());
    let enter_msg = with_header(HeaderKind::Enter, credential.enter_world());
    let bet_msg = with_header(HeaderKind::Normal, cross_server_war(9000012, 50000));

    let mut stream = TcpStream::connect(server)?;

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

fn main() -> Result<(), std::io::Error> {
    let credentials = Credential::cfg_credentials()?;

    for credential in credentials {
        thread::spawn(|| write_stream(credential));
    }

    // Single thread
    // let credential = Credential::cfg_credential().unwrap();
    // write_stream(credential);

    thread::sleep(Duration::from_secs(10));
    println!("Packets sent, please wait 10 secs!");
    Ok(())
}
