use std::{net::SocketAddr, num::ParseIntError};

const SERVER_IP_01: [u8; 4] = [103, 172, 79, 136];
const SERVER_IP_02: [u8; 4] = [103, 172, 79, 137];

pub enum HeaderKind {
    Login = 1,
    Enter = 2,
    Normal = 3,
}

pub fn server_addr(cfg_server: u16) -> SocketAddr {
    let server_ip = match cfg_server {
        p if p > 10 => SERVER_IP_02,
        _ => SERVER_IP_01,
    };

    let port = 8000 + (cfg_server % 10);
    SocketAddr::from((server_ip, port))
}

/// Encode a packet with header
pub fn with_header(header_kind: HeaderKind, data: Box<[u8]>) -> Box<[u8]> {
    let len = data.len() as u8;
    let mut header = [len + 4, 00, 00, 255, len, 00];

    match header_kind {
        HeaderKind::Login => header[2] = 01,
        HeaderKind::Enter => header[2] = 02,
        HeaderKind::Normal => header[2] = 03,
    };

    [&header[..], data.as_ref()].concat().into_boxed_slice()
}

/// Decode hex string into bytes.
#[allow(dead_code)]
pub fn decode_hex(s: &str) -> Result<Vec<u8>, ParseIntError> {
    (0..s.len())
        .step_by(2)
        .map(|i| u8::from_str_radix(&s[i..i + 2], 16))
        .collect()
}

#[cfg(test)]
mod tests {
    use crate::{
        config::{decode_hex, with_header, HeaderKind},
        login::Credential,
    };

    #[test]
    fn login_with_header() {
        let sniffed = "4f0001ff4b004c4f47494e20302e302e3120322034206861686168613036203132332031363438323935323937203162373863326236636231663335333338653365393034376239663361373730203000";
        let login_data = Credential::cfg_credential().unwrap();
        let with_header = with_header(HeaderKind::Login, login_data.login());
        assert_eq!(with_header, decode_hex(sniffed).unwrap().into_boxed_slice());
    }
}
