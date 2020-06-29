package main

func main() {
if a() {
b()
}
if a := b(); c {
d()
}
if a() {
b()
} else {
c()
}
if num := 9; num < 0 {
d()
} else if num < 10 {
e()
} else // comment
if f() {
  g()
}
}
