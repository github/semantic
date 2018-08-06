class LambdaTest {
    void singleton() {
      stateOwner.add(x -> System.out.println("State changed"));
    }
}
