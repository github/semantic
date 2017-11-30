package main

func main() {
select {
  case x := <-c:
    println(x)
  case y <- c:
    println(5)
  case <-time.After(1):
    println(6)
  	n--
  	if x {
  		waitDone <- result{n, x}
  		return
  	}
  default:
    return
}
select {
case <-ch:
case <-timeout:
default:
}
}
