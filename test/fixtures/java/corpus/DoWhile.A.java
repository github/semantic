class WhileDemo {
    public static void main(String[] args){
      do {
          System.out.print("Guess my name: ");
          guess = scanner.nextLine();
        }
        while (!"Daffy Duck".equals(guess));
    }
}
