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

// Attributes and expressions

fn foo() {
   bar(x,
       #[cfg(foo = "bar")]
       y);
   let z = [#[hello] 2, 7, 8];
   let t = (#[hello] 2, 7, 8);
}

// Enums

pub enum Option<T> {
    None,
    Some(T),
}

pub enum Node<T: Item> {
    Internal {
        children: Vec<Tree<T>>,
        height: u16
    },
    #[attribute1]
    #[attribute2]
    Leaf {
        value: T
    }
}

// Structs

struct Proton;
struct Electron {}
struct Person {pub name: String, pub age: u32}
struct Point {
  x: i32,

  #[attribute1]
  y: i32,
}
struct Color(pub i32, i32, i32);
struct Inches(i32);
