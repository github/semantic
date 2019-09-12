public class Bar {
  public void foo() {
   try (BufferedReader br = new BufferedReader()) {
      System.out.println(1/0);
    } catch (Exception e) {
      e.printStackTrace(System.out);
    }
  }
}
