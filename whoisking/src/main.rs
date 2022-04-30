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

fn main() {
    let credential = Credential::cfg_credential().unwrap();
    let server = server_addr(8000 + credential.cfg_server);

    let login_msg = with_header(HeaderKind::Login, credential.login());
    let enter_msg = with_header(HeaderKind::Enter, credential.enter_world());
    let bet_msg = with_header(HeaderKind::Normal, cross_server_war(9000012, 50000));

    let mut stream = TcpStream::connect(server).expect("Cannot bind tcp stream");

    stream.write(&login_msg).expect("Cannot login");
    stream.read(&mut [0; 256]).expect("Cannot recv login msg");

    stream.write(&enter_msg).expect("Cannot enter");
    stream.read(&mut [0; 80]).expect("Cannot recv enter msg");

    thread::sleep(Duration::from_secs(3));

    // Performance: 7x accepted
    for _ in 1..100 {
        stream.write(&bet_msg).expect("bet failed");
    }

    println!("End of program!");
}
