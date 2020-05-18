class Test {
    static void printCollection(Collection<?> c) {
          // a wildcard collection
        for (Object o : c) {
            System.out.println(o);
        }
    }
  }
