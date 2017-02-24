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
}
