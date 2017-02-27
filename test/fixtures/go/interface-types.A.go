package main

func main() {
type i1 interface {}
type i2 interface { io.Reader }
type i3 interface {
i1
io.Reader
 SomeMethod(s string) error
}
}
