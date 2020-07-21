// Modules

mod english;

mod english {}

mod english {
    mod greetings {}
    mod farewells {}
}

pub mod english;

// Function declarations

fn main() {}

fn add(x: i32, y: i32) -> i32 {
    return x + y;
}

fn takes_slice(slice: &str) {
    println!("Got: {}", slice);
}

fn foo() -> [u32; 2] {
    return [1, 2];
}

fn foo() -> (u32, u16) {
    return (1, 2);
}

fn foo() {
    return
}

fn foo(x: impl FnOnce() -> result::Result<T, E>) {}

fn foo(#[attr] x: i32, #[attr] x: i64) {}

fn accumulate(self) -> Machine<{State::Accumulate}> {}

