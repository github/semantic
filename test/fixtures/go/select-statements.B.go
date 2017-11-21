package main

func main() {
select {
  case a := <-c:
    println(x)
  case b <- c:
    println(5)
  case <-time.After(2):
    println(6)
  	n++
  	if v {
  		waitDone <- result{n, v}
  		return
  	}
  default:
    return
}
select {
case <-channel:
case <-timeout:
}
}
