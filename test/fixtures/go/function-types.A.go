package main

func main() {
	type (
		a func(int) int
		b func(int, string) (bool, error)
	)
}
