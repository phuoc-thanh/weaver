/// Player IDS:
///
/// PhuBich: 9000012
/// TrieuLinh s1: 101
/// mauvi s1: 355
/// BlackGO s5: 5000245
/// Cocain s5: 5000009
/// LamQuang s9: 9000138
/// Linh s4: 4000280
/// Embe s5: 5000058
///
/// Anh Hai s11: 110022
/// KhauThat s13: 130046
pub fn cross_server_war(pid: u32, amount: u32) -> Box<[u8]> {
    let prefix = "crossserverwar betting ";
    let player = format!("{}{}", pid, " ");
    let amount = format!("{}", amount);
    let suffix = [00];

    [
        prefix.as_bytes(),
        player.as_bytes(),
        amount.as_bytes(),
        suffix.as_slice(),
    ]
    .concat()
    .into_boxed_slice()
}

#[allow(dead_code)]
pub fn rank_reward(rank: u8) -> Box<[u8]> {
    let prefix = "rankList reward ";
    let rank = format!("{}", rank);
    let suffix = [00];

    [prefix.as_bytes(), rank.as_bytes(), suffix.as_slice()]
        .concat()
        .into_boxed_slice()
}
