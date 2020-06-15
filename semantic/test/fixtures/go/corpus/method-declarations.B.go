package main

func main() {}

func (s) Methods()

func (self Num) Equals(other Num) bool {}

func (p *Point) OtherLength() float64 {
	return math.Sqrt(math.Pow(p.x, 2) + p.x + math.Pow(p.y, 2) + p.y)
}

func (q *Point) Scale(factor int) {
	p.x *= factor
	p.y *= factor
}

func (f *Field) Alive(z, h int) bool { }
