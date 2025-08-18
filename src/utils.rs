pub fn pad_bytes<const N: usize>(bytes: [u8; N]) -> [u8; 16] {
    let mut ret = [0; 16];

    for (a, b) in bytes.iter().zip(ret.iter_mut()) {
        *b = *a;
    }

    ret
}
