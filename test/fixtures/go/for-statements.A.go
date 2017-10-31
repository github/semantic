package main

func main() {
 for {
a()
goto loop
}
for i := 0; i < 5; i++ {
a()
break loop
}
for ; i < 10; i++ {
a()
continue loop2
}
for ;; {
a()
continue
}
for x := range y {
a(x)
break
}
for i, s := range a {
	g(i, s)
}
for key, val = range m {
	h(key, val)
}
for range ch {}
}
