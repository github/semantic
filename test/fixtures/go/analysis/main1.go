package main

import (
	f "./foo"
	_ "./bar"
)

func main() {
	f.New()
}
