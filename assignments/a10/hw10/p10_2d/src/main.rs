use std::io;

fn main() {
    let mut input = String::new();
    let mut input2 = String::new();
    let mut i :u16;
    let mut data :i32;

    println!("Please input number of iterations: ");
    io::stdin()
        .read_line(&mut input)
        .expect("Failed to get number of iterations!");
    i = input.trim().parse().expect("Please insert a number!");

    println!("Please input starting value of data: ");
    io::stdin()
        .read_line(&mut input2)
        .expect("Failed to get starting value of data!");
    data = input2.trim().parse().expect("Please insert a number!");

    loop {
        if i == 0 {
            break;
        }

        data *= 2;

        i -= 1;
    }

    println!("Data is: {data}");
}
