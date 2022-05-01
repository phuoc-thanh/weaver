use std::{
    fs::File,
    io::{BufReader, Read},
};

use serde::{Deserialize, Serialize};

/// Login String packet struture:
///
/// PacketHeader - First 6 bytes:
/// Length of packet = Length of data + 4.
/// [Length of packet] - [00] - [PacketSerial] - [ff] - [Length of data] - [00]
///
/// PacketData - Remaining bytes.
/// [LoginPrefix] - [20] - [GameServerNo] - [20] - [Username] - [20] - [31,32,33] - [20] - [timestamp] - [20] - [hash] - [20] - [30] - [00]
const LOGIN_PREFIX: &'static str = "LOGIN 0.0.1 2";

#[derive(Debug, Default, Serialize, Deserialize)]
pub struct Credential {
    username: String,
    userid: u32,
    pub cfg_server: u16,

    // Dynamic Parts, recv from a http call to auth server
    // Cannot capture these info

    // Receive from a TLS session (443)
    pub timestamp: i64,

    // Receive from a TLS session (443)
    // What is this? Seem like a md5 hash?
    pub hash_str: String,
}

impl Credential {
    /// Packet Structure of Login:
    ///
    /// [LoginPrefix] [ ] [GameServerNo] [ ] [Username] [ ] [123] [ ] [timestamp] [ ] [hash] [ ] [0null]
    pub fn login(&self) -> Box<[u8]> {
        let prefix = format!("{}{}", LOGIN_PREFIX, " ");
        let server_str = format!("{}{}", self.cfg_server, " ");
        let username = format!("{}{}", self.username, " ");
        let str_123 = "123 ".to_owned();
        let timestamp = format!("{}{}", self.timestamp, " ");
        let hash_str = format!("{}{}", self.hash_str, " ");
        let suffix = [48, 00]; // 48 = "0" in ascii, or "30" in hex string

        [
            prefix.as_bytes(),
            server_str.as_bytes(),
            username.as_bytes(),
            str_123.as_bytes(),
            timestamp.as_bytes(),
            hash_str.as_bytes(),
            suffix.as_slice(),
        ]
        .concat()
        .into_boxed_slice()
    }

    /// Read credential.json file to get a Credential.
    #[allow(dead_code)]
    pub fn cfg_credential() -> Result<Credential, serde_json::Error> {
        let file = File::open("credential.json").unwrap();
        let mut contents = String::new();
        let mut buf_reader = BufReader::new(file);

        buf_reader.read_to_string(&mut contents).unwrap();
        serde_json::from_str(&contents)
    }

    /// Read credentials.json file to get multiples Credentials.
    #[allow(dead_code)]
    pub fn cfg_credentials() -> Result<Vec<Credential>, serde_json::Error> {
        let file = File::open("credentials.json").unwrap();
        let mut contents = String::new();
        let mut buf_reader = BufReader::new(file);

        buf_reader.read_to_string(&mut contents).unwrap();
        serde_json::from_str(&contents)
    }

    pub fn enter_world(&self) -> Box<[u8]> {
        let prefix = "ENTER ".as_bytes();
        let username = format!("{}{}", self.username, " ");
        let userid = format!("{}{}", self.userid, " ");
        let suffix = [00];

        [
            prefix,
            username.as_bytes(),
            userid.as_bytes(),
            suffix.as_slice(),
        ]
        .concat()
        .into_boxed_slice()
    }
}
