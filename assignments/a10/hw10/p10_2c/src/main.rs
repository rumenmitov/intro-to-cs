fn main() {
    let mut m :[u8; 16] = 
        [14, 0, 4, 0, 15, 15, 15, 14, 1, 14, 11, 0, 0, 0, 6, 1];

    let mut acc = m[14];

    loop {
        if acc == 0 {
            break;
        }

        acc = m[15];
        acc += m[15];

        m[15] = acc;

        acc = m[14];
        acc -= 1;
        m[14] = acc;
    }

    println!("Data is: {}", m[15]);
}
