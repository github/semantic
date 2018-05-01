package main

func main() {
type (
c1 chan<- chan int
c2 chan<- chan<- struct{}
c3 chan<- <-chan int
c4 <-chan <-chan int
c5 chan (<-chan int)
)
}
