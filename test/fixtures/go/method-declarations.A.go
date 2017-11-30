package main

func main() {}

func (s) Method()

func (self Person) Equals(other Person) bool {}

func (p *Point) Length() float64 {
	return math.Sqrt(p.x * p.x + p.y * p.y)
}

func (p *Point) Scale(factor float64) {
	p.x *= factor
	p.y *= factor
}

func (f *Field) Alive(x, y int) bool { }
