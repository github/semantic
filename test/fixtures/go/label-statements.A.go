package main

func main() {
L: ;  // ';' terminates empty statement => L does not apply to for loop
L1: { // L1 labels block => L1 does not apply to for loop
	for i := 0; i < 10; i++ {
		println(i);
		break L1;  // comment
	}
}
{
  insert:
}
}
