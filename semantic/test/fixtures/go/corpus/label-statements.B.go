package main

func main() {
M: ;  // ';' terminates empty statement => M does not apply to for loop
M1: { // M1 labels block => M1 does not apply to for loop
	for i := 0; i < 10; i++ {
		println(i);
		break M1;  // comment
	}
}
{
  replacement:
}
}
