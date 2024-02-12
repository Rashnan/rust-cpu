fn main() {
    let v = -8;
    let n = v as i8 as i128 as u128 as i128;
    println!("i want 8b:\n{} --> 0x{:x}", n, n);
}
