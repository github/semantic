package main

func main() {
	type (
		x func(string) string
		y func(string, int) (chan x, error)
	)
}
