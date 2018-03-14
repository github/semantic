package app

import _ "lib/Foo"
import . "lib/Bar"
import m "lib/Math"
import "lib/Math"

import (
	"net/http"
)

func foo() {}

func main() {
	foo()

	m.Sin()
	Math.Sin()
}
