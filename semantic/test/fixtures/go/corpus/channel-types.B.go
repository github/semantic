package main

func main() {
type (
c2 chan<- chan string
c3 chan<- chan<- struct{}
c4 chan<- <-chan string
c4 <-chan <-chan string
c5 chan (<-chan string)
)
}
